# Uganda precipitation
# Investigate daily rainfall (mm/day) in Uganda using two datasets CHIRPS and FEWS/ARC2
# Linh Ho (WEMC) - 26/03/2019

rm(list = ls())

# install.packages("A")  # only run for the first time for each package A below  
library(rgdal)
library(ggplot2)
library(mapproj)
library(lubridate)  # use pipe
library(tidyverse)
library(rnoaa)      # NOAA packages
library(raster)     # read raster files
library(R.utils)    # read tif files
library(rts)        # raster time series
library(RColorBrewer)

# install.packages("devtools")
library(devtools)
# install_github("environmentalinformatics-marburg/chirps")
# install_github("environmentalinformatics-marburg/heavyRain")
library(heavyRain)

select <- dplyr::select # avoid conflict with 'select' function from other packages

#*** Define home directory, adapt to your own structure

baseDir <- "D:/WEMC/Uganda_precipitation"
setwd(baseDir)
PATH_DATA <- file.path(baseDir, "DATA")
PATH_FIGURE <- file.path(baseDir, "FIGURE")
PATH_shapefile <- file.path(PATH_DATA, "Shapefiles")
PATH_CHIRPS <- file.path(PATH_DATA, "CHIRPS_tif")
PATH_ARC2 <- file.path(PATH_DATA, "ARC2")

### ============= Get the shapefiles for Uganda  =========================================
#
# Available at https://data.humdata.org/dataset/uganda-administrative-boundaries-admin-1-admin-3 
# Make sure all of the raster data use the same projection referece:
# coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 

require(rgdal)
# read the shapefile with the extension .shp

# Level 0 (country)
PATHs <- file.path(PATH_shapefile, "uga_admbnda_adm0_ubos_v2")
Uganda_lev0 <- readOGR(dsn = PATHs, layer = "uga_admbnda_adm0_UBOS_v2")  

# Level 1 (district)
PATHs <- file.path(PATH_shapefile, "uga_admbnda_adm1_ubos_v2")
Uganda_lev1 <- readOGR(dsn = PATHs, layer = "uga_admbnda_adm1_UBOS_v2")  

# Level 3 (sub-county)
PATHs <- file.path(PATH_shapefile, "uga_admbnda_adm3_ubos_v5")
Uganda_lev3 <- readOGR(dsn = PATHs, layer = "uga_admbnda_adm3_UBOS_v5") 

level_chosen <- Uganda_lev3  # select your desired level of shapefile
LABEL <- "Level_1"  # make sure to create a corresponding folder in FIGURE to store plots

### ============= Read meteorological data ================================================

#### ========= CHIRPS ==========
#
# Information at http://chg.geog.ucsb.edu/data/chirps/
# Select daily African precipitation data at 0.05 degree resolution, period 1981 to present
#*** Starting and ending dates of the examined period, modify if needed

CHIRPS_START  <- as.Date("2000-07-01")
CHIRPS_END    <- as.Date("2000-07-31")
CHIRPS_PERIOD <- seq(from = as.Date(CHIRPS_START), to = as.Date(CHIRPS_END), by = 1)

# Get CHIRPS data
getCHIRPS(region =  "africa", format = "tifs", tres = "daily", sres = 0.05, begin = as.Date(CHIRPS_START), end = as.Date(CHIRPS_END), 
          dsn = file.path(PATH_CHIRPS))


# Unzip the files downloaded from ftp to GeoTIFF format in the same folders

for (i in 1:length(CHIRPS_PERIOD)) {
  # YEARs <- year(CHIRPS_PERIOD[i])
  tmp_day <- format(as.Date(CHIRPS_PERIOD[i]), "%Y.%m.%d")
  # filename <- paste0(PATH_CHIRPS,"/", YEARs, "/chirps-v2.0.", tmp_day, ".tif")  # if you download directly from FTP, they will be sorted by year, uncomment this line and the YEARs line
  filename <- paste0(PATH_CHIRPS,"/", "chirps-v2.0.", tmp_day, ".tif")
  
  # Only unzip new files and skip tif files already exist
  if (file.exists(filename)) { next
  } else {
    filename <- paste0(filename, ".gz")
    gunzip(filename, overwrite = FALSE, remove = FALSE)  # overwrite/remove = TRUE to overwrite/remove the original file and replace with the unzipped tif file 
  }
}
rm(tmp_day, filename, YEARs)

# Read tif file into raster format, extract the mean values for each region in shapefile
# then save them to a dataframe Uganda_df
#*** Change to your appropriate shapefile level

Uganda_shapefile <- level_chosen
Uganda_df <- data.frame()

for (i in 1:length(CHIRPS_PERIOD)) {
  YEARs <- year(CHIRPS_PERIOD[i])
  tmp_file <- paste0(PATH_CHIRPS, "/chirps-v2.0.", format(as.Date(CHIRPS_PERIOD[i]), "%Y.%m.%d"), ".tif")
  
  tmp_raster <- raster(tmp_file)
  tmp_raster[tmp_raster==-9999] <- NaN  # ocean cells
  
  tmp_extract <- raster::extract(tmp_raster, Uganda_shapefile) # Extract raster values for each polygon and check object class
  tmp_means <- unlist( lapply(tmp_extract, mean) )  # Use lapply to summarize polygon raster values to mean
  Uganda_shapefile@data["means"] <- tmp_means
  # Extract the dataframe of mean for each feature/administrative areas
  tmp_df <- Uganda_shapefile@data %>% mutate(Date = CHIRPS_PERIOD[i]) %>%  # add corresponding date
            mutate(Area_ID = row_number())  # number the administrative areas
  Uganda_df <- bind_rows(Uganda_df, tmp_df)
  
}
rm(tmp_file, tmp_raster, tmp_extract, tmp_means, tmp_df)  # clean the mess

# Reshape the dataframe into desired tabular format
Uganda_df <- Uganda_df %>%
  select(-ADM0_EN, -ADM0_PCODE, CHIRPS_precipitation_mm = means, -contains("OBJECTID")) %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>% arrange(Area_ID)
o <- length(Uganda_df)
Uganda_df <- Uganda_df[,c(o-3, o-2, o-1, o, o-4, 1:(o-5))]  # arrange column in order

Uganda_TP_CHIRPS_S19900101_E19900110_daily <- Uganda_df  # save the result in a separate variable and rda file
#*** Change the name accordingly
save(Uganda_TP_CHIRPS_S19810101_E20190331_daily, file = file.path(PATH_DATA, 'Uganda_TP_CHIRPS_S19810101_E20190331_daily.rda'))

#### ========= FEWS/ARC2 ===========
#
# Available at  ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2
# FEWS is a global dataset from NOAA, ARC2 (Africa Rainfall Climatology version 2) is a subset of FEWS
# Select daily African precipitation data at 0.1 degree resolution, period 1983 to present
# See ARC2_missing_dates.txt for missing values due to unavailablity of gauge and/or IR input data from 1983-2013

# Starting and ending dates of the examined period, modify if needed

ARC2_START  <- as.Date("1983-01-01")
ARC2_END    <- as.Date("2019-03-31")
ARC2_PERIOD <- seq(from = as.Date(ARC2_START), to = as.Date(ARC2_END), by = 1)

# Define a rectangle to cover Uganda for faster processing
Uganda_rect <- data.frame(W = 29.5, E = 35.25, S = -1.5, N = 4.5)

Uganda_df <- data.frame()
Uganda_shapefile <- level_chosen

for (i in 1:length(ARC2_PERIOD)) {
  
  tmp_df <- arc2(date = ARC2_PERIOD[i])  # get the data from ftp of NOAA
  
  # Crop only rectangle covering Uganda 
  tmp_df_sel <- tmp_df %>% filter(lon >= Uganda_rect$W  & lon <= Uganda_rect$E & lat >= Uganda_rect$S & lat <= Uganda_rect$N)
  tmp_raster <- rasterFromXYZ(tmp_df_sel)
  crs(tmp_raster) <- "+proj=longlat +datum=WGS84 +no_defs"  # make sure raster data have the same projection reference
  tmp_raster[tmp_raster==-999] <- NaN  # ocean or missing values
  
  tmp_extract <- raster::extract(tmp_raster, Uganda_shapefile) # Extract raster values for each polygon and check object class
  tmp_means <- unlist( lapply(tmp_extract, mean) )  # Use lapply to summarize polygon raster values to mean
  Uganda_shapefile@data["means"] <- tmp_means
  # Extract the dataframe of mean for each feature/administrative areas 
  tmp_df <- Uganda_shapefile@data %>% mutate(Date = ARC2_PERIOD[i]) %>%  # add corresponding date
            mutate(Area_ID = row_number())  # number the administrative areas
  Uganda_df <- bind_rows(Uganda_df, tmp_df)
  
}
rm(tmp_df, tmp_df_sel, tmp_raster, tmp_extract, tmp_means)

# Reshape the dataframe into desired tabular format

Uganda_df <- Uganda_df %>%
  select(-ADM0_EN, -ADM0_PCODE, ARC2_precipitation_mm = means, -contains("OBJECTID")) %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>% arrange(Area_ID)
o <- length(Uganda_df)
Uganda_df <- Uganda_df[,c(o-3, o-2, o-1, o, o-4, 1:(o-5))]  # arrange column in order

#*** Change the filename accordingly
Uganda_TP_ARC2_S19900101_E19900110_daily <- Uganda_df  # save the result in a separate variable and rda file
save(Uganda_TP_ARC2_S19830101_E20190331_daily, file = file.path(PATH_DATA, 'Uganda_TP_ARC2_S19830101_E20190331_daily.rda'))

Uganda_precipitation_CHIRPS_ARC2 <- left_join(Uganda_TP_CHIRPS_S19900101_E19900110_daily, Uganda_TP_ARC2_S19900101_E19900110_daily)

# save in rda format for quicker use later
save(Uganda_precipitation_CHIRPS_ARC2, file = file.path(PATH_DATA, 'Uganda_precipitation_CHIRPS_ARC2.rda'))

# Save the tabular dataframe in csv file in your home directory, use getwd() to find out
write.csv(Uganda_precipitation_CHIRPS_ARC2, "Uganda_precipitation_CHIRPS_ARC2.csv", row.names = FALSE, quote = FALSE )


### ============ Analyse the data =============================================================================
#
# Plot for each of 122 districts

for (i in 1:length(Uganda_shapefile@polygons)) {
  plot_file <- paste0(PATH_FIGURE,"/", LABEL, "/Uganda_TP_",i,".png")
  png(plot_file, width=10, height=7, units="in", res=100)
  tmp <- Uganda_precipitation_CHIRPS_ARC2 %>% filter(Area_ID == i)
  
  # Time series of CHIRPS and ARC2 data
  par(mfrow = c(2,1), mar = c(2,4,2,4))
  plot(CHIRPS_precipitation_mm ~ Date, data = tmp, type = "l", col = "steelblue", ylab = "Precipitation (mm)")
  lines(ARC2_precipitation_mm ~ Date, data = tmp, col = "red")
  title(paste0("Area ", i, " (", tmp[1, length(tmp)-3], " - ", tmp[1, length(tmp)-2], ")" ))
  
  # Density plot of distribution of precipitation over the period
  # Fix the y-axis limit for easier comparison, however, some CDF values exceed 0.5, adjust the ylim if needed
  plot(density(tmp$CHIRPS_precipitation_mm), col = "steelblue", ylim = c(0,0.5), main = NA, lwd = 1.8)  # check the district 108 has CDF > 1
  lines(density(tmp$ARC2_precipitation_mm, na.rm = TRUE), col = "red", lwd = 1.8)  # remove the NA values in ARC2
  legend("topright", legend=c("CHIRPS", "ARC2"), ncol = 2, lty=c(1,1), lwd=c(5,5), col=c( "steelblue", "red"))
  dev.off()
}

### Mapping the statistical value of precipitation by district ================================================
#
#*** Change the plot title (main) and variable name according to your needs

## ========== Annual average (mm/year) ======================

# Calculate annual average per district

# CHIRPS
toplot_CHIRPS <- Uganda_precipitation_CHIRPS_ARC2 %>% select(-ARC2_precipitation_mm) %>% na.omit() %>%
  group_by(Area_ID, Year) %>%
  summarise(total_peryear = sum(CHIRPS_precipitation_mm)) %>% mutate_if(is.factor, as.numeric) %>%  # total precipitation every year of each district
  group_by(Area_ID) %>%
  summarise(annual_avg_CHIRPS = mean(total_peryear)) %>% mutate_if(is.factor, as.numeric) %>%  # average of annual precipitation for each district over the whole period
  arrange(Area_ID)

Uganda_shapefile@data <- toplot_CHIRPS
my.palette <- brewer.pal(n = 9, name = "YlGnBu")  # colour the map, area with highest precipitation is in deep blue

png(file.path(PATH_FIGURE, "Uganda_CHIRPS_avg_test.jpeg"), units="in", width=8, height=8, res=150)
spplot(Uganda_shapefile, "annual_avg_CHIRPS", col.regions = my.palette, cuts = 8, 
         main = "Uganda annual average precipitation - CHIRPS 1990-01 test (mm/year)")
dev.off()

# ARC2
toplot_ARC2 <- Uganda_precipitation_CHIRPS_ARC2 %>% select(-CHIRPS_precipitation_mm) %>% na.omit() %>%
  group_by(Area_ID, Year) %>%
  summarise(total_peryear = sum(ARC2_precipitation_mm)) %>% mutate_if(is.factor, as.numeric) %>%  # total precipitation every year of each district
  group_by(Area_ID) %>%
  summarise(annual_avg_ARC2 = mean(total_peryear)) %>% mutate_if(is.factor, as.numeric) %>%  # average of annual precipitation for each district over the whole period
  arrange(Area_ID)

Uganda_shapefile@data <- toplot_ARC2

png(file.path(PATH_FIGURE, "Uganda_ARC2_avg_1990_test.jpeg"), units="in", width=8, height=8, res=150)
spplot(Uganda_shapefile, "annual_avg_ARC2", col.regions = my.palette, cuts = 8, 
       main = "Uganda annual average precipitation - ARC2 1990 test (mm/year)")
dev.off()

# Difference between CHIRPS and ARC2 data

tocompare <- left_join(toplot_CHIRPS, toplot_ARC2) %>% mutate(CHIRPS_ARC2_difference = annual_avg_CHIRPS - annual_avg_ARC2)
Uganda_shapefile@data <- tocompare
my.palette_compare <- brewer.pal(n = 9, name = "RdYlBu")  # colour the map, area with highest precipitation is in deep blue

png(file.path(PATH_FIGURE, "Uganda_compare_CHIRPS_ARC2_avg_1990_test.jpeg"), units="in", width=8, height=8, res=150)
spplot(Uganda_shapefile, "CHIRPS_ARC2_difference", col.regions = my.palette_compare, cuts = 8, 
       main = "Uganda annual average difference CHIRPS - ARC2 test (mm/year)")
dev.off()

rm(toplot_ARC2, toplot_CHIRPS, tocompare)

## ========== Average of yearly maximum (mm/day) ======================

# Calculate average of yearly maximum per district

# CHIRPS
toplot_CHIRPS <- Uganda_precipitation_CHIRPS_ARC2 %>% select(-ARC2_precipitation_mm) %>% na.omit() %>%
  group_by(Area_ID, Year) %>%
  summarise(max_byyear = max(CHIRPS_precipitation_mm)) %>% mutate_if(is.factor, as.numeric) %>%  # maximum precipitation every year of each district
  group_by(Area_ID) %>%
  summarise(avg_yearlymax_CHIRPS = mean(max_byyear)) %>% mutate_if(is.factor, as.numeric) %>%  # average of maximum precipitation for each district over the whole period
  arrange(Area_ID)

Uganda_shapefile@data <- toplot_CHIRPS

png(file.path(PATH_FIGURE, "Uganda_CHIRPS_max_1990_test3.jpeg"), units="in", width=8, height=8, res=150)
spplot(Uganda_shapefile, "avg_yearlymax_CHIRPS", col.regions = my.palette, cuts = 8, 
       main = "Uganda average of yearly max precipitation - CHIRPS 1990 test (mm/day)")
dev.off()

# ARC2
toplot_ARC2 <- Uganda_precipitation_CHIRPS_ARC2 %>% select(-CHIRPS_precipitation_mm) %>% na.omit() %>%
  group_by(Area_ID, Year) %>%
  summarise(max_byyear = max(ARC2_precipitation_mm)) %>% mutate_if(is.factor, as.numeric) %>%  # maximum precipitation every year of each district
  group_by(Area_ID) %>%
  summarise(avg_yearlymax_ARC2 = mean(max_byyear)) %>% mutate_if(is.factor, as.numeric) %>%  # average of maximum precipitation for each district over the whole period
  arrange(Area_ID)

Uganda_shapefile@data <- toplot_ARC2

png(file.path(PATH_FIGURE, "Uganda_ARC2_max_1990_test.jpeg"), units="in", width=8, height=8, res=150)
spplot(Uganda_shapefile, "avg_yearlymax_ARC2", col.regions = my.palette, cuts = 8, 
       main = "Uganda average of yearly max precipitation - ARC2 1990 test (mm/day)")
dev.off()

# Difference between CHIRPS and ARC2 data

tocompare <- left_join(toplot_CHIRPS, toplot_ARC2) %>% mutate(CHIRPS_ARC2_difference = avg_yearlymax_CHIRPS - avg_yearlymax_ARC2)
Uganda_shapefile@data <- tocompare

png(file.path(PATH_FIGURE, "Uganda_compare_CHIRPS_ARC2_max_1990_test.jpeg"), units="in", width=8, height=8, res=150)
spplot(Uganda_shapefile, "CHIRPS_ARC2_difference", col.regions = my.palette_compare, cuts = 8, 
       main = "Uganda average yearly max difference CHIRPS - ARC2 test 1990 (mm/day)")
dev.off()

