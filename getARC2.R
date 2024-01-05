## Uganda precipitation
# Get FEWS data from NOAA - ARC2 (Africa Rainfall Climatology version 2)
# Africa daily 0.1 deg and mask to Uganda shapefile
# @author: Linh Ho (WEMC) - 26/03/2019

# Download function to get ARC2 data from rnoaa package
# Link: https://rdrr.io/cran/rnoaa/src/R/arc2.R 

# install.packages("rnoaa")
library(rnoaa)
library(raster)
library(lubridate)
library(tidyverse)


baseDir <- "D:/WEMC/Uganda_precipitation"
setwd(baseDir)

PATH_DATA <- file.path(baseDir, "DATA")
PATH_FIGURE <- file.path(baseDir, "FIGURE")
PATH_ARC2 <- file.path(PATH_DATA, "ARC2")

# retrieve data from FEWS/ARC2 for Africa

ID_START <- as.Date("1989-01-01")
ID_END <- as.Date("1990-12-31")
ID_PERIOD <- seq(from = ID_START, to = ID_END, by = 1)

# Uganda rectangle
Uganda_rectangle <- data.frame(W = 29.5, E = 35.25, S = -1.5, N = 4.5)

Uganda_df <- data.frame()
tmp_Uganda_lev1 <- Uganda_lev1

for (i in 1:length(ID_PERIOD)) {
  
  tmp_df <- arc2( date = ID_PERIOD[i]) 
  # Crop only rectangle covering Uganda 
  tmp_df_sel <- tmp_df %>% filter(lon >= Uganda_W  & lon <= Uganda_E & lat >= Uganda_S & lat <= Uganda_N)
  tmp_raster <- rasterFromXYZ(tmp_df_sel)
  crs(tmp_raster) <- "+proj=longlat +datum=WGS84 +no_defs"  # make sure raster data have the same projection reference
  tmp_raster[tmp_raster==-999] <- NaN

  Uganda_extract <- raster::extract(tmp_raster, Uganda_lev1) # Extract raster values for each polygon and check object class
  tmp_means <- unlist( lapply(Uganda_extract, mean) )  # Use lapply to summarize polygon raster values to mean
  tmp_Uganda_lev1@data["means"] <- tmp_means
  tmp_df <- tmp_Uganda_lev1@data %>% mutate(Date = ID_PERIOD[i])  # Extract the dataframe of mean for each feature/administrative areas and write corresponding date
  Uganda_df <- bind_rows(Uganda_df, tmp_df)

}

crs(tmp_raster) <- "+proj=longlat +datum=WGS84 +no_defs"
mymap <- rasterFromXYZ(tmp_raster)

plot(mymap)






install.packages("urllib2")
library(urllib)
urlretrieve("ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2/geotiff/", "folder link")

library(RCurl)
library(curl)
url <- "https://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2/geotiff/" 

destdir <- PATH_ARC2  # Adapt to your system 
lapply(fn, function(x) download.file(paste0(url, x), paste0(destdir, x))) 

