# Uganda precipitation
# Get CHIRPS data - Africa daily 0.05 deg and mask to Uganda shapefile
# @author: Linh Ho (WEMC) - 26/03/2019

# Download function to get CHIRPS data from GitHub
# Link: https://github.com/environmentalinformatics-marburg/heavyRain/blob/master/R/getCHIRPS.R

# Use intall.packages("A") to install the necessary packages for the first use
# install.packages("devtools")
library(devtools)
install_github("environmentalinformatics-marburg/chirps")
# install_github("environmentalinformatics-marburg/heavyRain")
library(heavyRain)

getCHIRPS(region =  "africa", format = "tifs", tres = "daily", sres = 0.05, begin = as.Date("1981-01-01"), end = as.Date("1981-12-31"), 
          dsn = file.path(PATH_DATA, "CHIRPS_test")) # add overwrite option

rm(list = ls())

library(ggplot2)
library(mapproj)
# library(zoo)
# library(reshape2)
# library(chron)
# library(seas)
library(lubridate)
library(tidyverse)
library(raster)  # read raster files
library(R.utils)  # read tif files
library(rts)  # raster time series
library(rgdal)
library(RColorBrewer)
# library(tiff)
# library(maptools)
select <- dplyr::select # avoid conflict with 'select' function from other packages

baseDir <- "D:/WEMC/Uganda_precipitation"
setwd(baseDir)

PATH_DATA <- file.path(baseDir, "DATA")
PATH_FIGURE <- file.path(baseDir, "FIGURE")
PATH_shapefile <- file.path(PATH_DATA, "Shapefiles")
PATH_CHIRPS <- file.path(PATH_DATA, "CHIRPS_tif")

### read shapefile ========================================================================
require(rgdal)

# coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
PATHs <- file.path(PATH_shapefile, "uga_admbnda_adm1_ubos_v2")
Uganda_lev1 <- readOGR(dsn = PATHs, layer = "uga_admbnda_adm1_UBOS_v2")  # read the shapefile with the extension part .shp

# coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
PATHs <- file.path(PATH_shapefile, "uga_admbnda_adm3_ubos_v5")
Uganda_lev3 <- readOGR(dsn = PATHs, layer = "uga_admbnda_adm3_UBOS_v5")  # read the shapefile with the extension part .shp

### Read meteorological data ==============================================================
#### =============================== CHIRPS ===============================================
# Starting and ending dates of the examined period, modify if needed

ID_START <- as.Date("1981-01-01")
ID_END <- as.Date("1990-12-31")
ID_PERIOD <- seq(from = as.Date(ID_START), to = as.Date(ID_END), by = 1)

# Unzip the files downloaded from ftp to GeoTIFF format

for (i in 1:length(ID_PERIOD)) {
  YEARs <- year(ID_PERIOD[i])
  tmp_day <- format(as.Date(ID_PERIOD[i]), "%Y.%m.%d")
  filename <- paste0(PATH_CHIRPS,"/", YEARs, "/chirps-v2.0.", tmp_day, ".tif")
  
  # Only unzip new files and skip tif files already exist
  
  if (file.exists(filename)) { next
  } else {
      filename <- paste0(filename, ".gz")
      tmp_unzip <- gunzip(filename, overwrite = FALSE, remove = FALSE)  # overwrite/remove = TRUE to overwrite/remove the original file and replace with the unzipped tif file 
    }
}


# Make sure the shapefile and raster have the same projection
# coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 

# Uganda_rec <- as(extent(29.5, 35.25, -1.5, 4.5), 'SpatialPolygons')
# crs(Uganda_rec) <- "+proj=longlat +datum=WGS84 +no_defs"

# all_CHIRPS <- list.files(PATH_CHIRPS, full.names = TRUE, pattern = ".tif$")
# all_CHIRPS

# Africa_stack <- stack(all_CHIRPS)

## 

Uganda_df <- data.frame()

for (i in 1:length(ID_PERIOD)) {
  YEARs <- year(ID_PERIOD[i])
  tmp_file <- paste0(PATH_CHIRPS,"/", YEARs, "/chirps-v2.0.", format(as.Date(ID_PERIOD[i]), "%Y.%m.%d"), ".tif")
  
  # read tif file into raster format and assign NaN to ocean grid cells
  
  tmp_raster <- raster(tmp_file)
  tmp_raster[tmp_raster==-9999] <- NaN
  
  Uganda_extract <- raster::extract(tmp_raster, Uganda_lev1) # Extract raster values for each polygon and check object class
  tmp_means <- unlist( lapply(Uganda_extract, mean) )  # Use lapply to summarize polygon raster values to mean
  Uganda_lev1@data["means"] <- tmp_means
  tmp_df <- Uganda_lev1@data %>% mutate(Date = ID_PERIOD[i])  # Extract the dataframe of mean for each feature/administrative areas and write corresponding date
  Uganda_df <- bind_rows(Uganda_df, tmp_df)

}

Uganda_TP_CHIRPS_S19810101_E19901231_daily <- Uganda_TP_CHIRPS_S19810101_E19901231_daily %>% mutate(District = as.numeric(OBJECTID)) %>%
  select(-OBJECTID, -ADM0_EN, -ADM0_PCODE, District, CHIRPS_precipitation_mm = means) %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>% arrange(District)
Uganda_TP_CHIRPS_S19810101_E19901231_daily <- Uganda_TP_CHIRPS_S19810101_E19901231_daily[,c(5,6,7,8,4,1,2,3)]
write.csv(Uganda_TP_CHIRPS_S19810101_E19901231_daily, "Uganda_TP_CHIRPS_S19810101_E19901231_daily.csv", row.names = FALSE, quote = FALSE )


for (i in 1:122) {
  plot_file <- paste0(baseDir,"/FIGURE/Uganda_TP_dist_",i,".png")
  png(plot_file, width=10, height=5, units="in", res=100)
  tmp <- Uganda_TP_CHIRPS_S19810101_E19811231_daily %>% filter(District == i)
  plot(CHIRPS_precipitation ~ Date, data = tmp, type = "l")
  title(paste0("District ", i, " (", tmp[["ADM1_EN"]][1], ")" ))
  # do.call(grid.arrange, c(plot_list[[cnt]][1:3], ncol = 1))
  dev.off()
}

bb <- Uganda_TP_CHIRPS_S19810101_E19811231_daily
bb <- bb %>% group_by(District) %>%
  summarise(Year_max = max(CHIRPS_precipitation)) %>% mutate_if(is.factor, as.numeric) %>% arrange(District)

tmp_Uganda_lev1@data <- bb



png("Uganda_ARC2_test.jpeg", units="in", width=8, height=8, res=150)
plot()
dev.off()

my.palette <- brewer.pal(n = 9, name = "YlGnBu")
spplot(tmp_Uganda_lev1, "means", col.regions = my.palette, cuts = 8, main = "Uganda ARC2 (mm)")



# Raster stack to make time series data

# CHIRPS_stack <- stack(all_CHIRPS)

# Check metadata
crs(CHIRPS_stack)  # coordinate reference system
CHIRPS_stack

plot(CHIRPS_stack)

hist(a_stack)
summary(a_stack)
avg_a_stack <- cellStats(a_stack, mean) %>% as.data.frame()

r.med <- raster::extract (Uganda_stack, Uganda_rec, fun = median, na.rm = TRUE)

for(i in 1:nrow(r.med)) {
  plot(seq(as.Date(ID_START), by="day", length.out = nlayers(Uganda_stack)), 
       r.med[i,], type="l", xlab="Median", ylab="Date",
       main=paste0("Polygon - ", i) ) 
}   
