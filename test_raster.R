# to test data CHIRPS and ARC2
# plot raster data with map superimposed
# Linh Ho (17/05/2019)

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
# library(rasterVis)

library(devtools)
library(heavyRain)

select <- dplyr::select # avoid conflict with 'select' function from other packages

baseDir <- "D:/WEMC/Uganda_precipitation"
setwd(baseDir)
PATH_DATA <- file.path(baseDir, "DATA")
PATH_FIGURE <- file.path(baseDir, "FIGURE")
PATH_shapefile <- file.path(PATH_DATA, "Shapefiles")
PATH_CHIRPS <- file.path(PATH_DATA, "CHIRPS_tif")
PATH_ARC2 <- file.path(PATH_DATA, "ARC2")

### ============= Get the shapefiles for Uganda  =========================================

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

level_chosen <- Uganda_lev1  # select your desired level of shapefile
LABEL <- "Level_1"  # make sure to create a corresponding folder in FIGURE to store plots

#### ========= CHIRPS ==========
#
#*** Starting and ending dates of the examined period, modify if needed

CHIRPS_START  <- as.Date("2000-07-01")
CHIRPS_END    <- as.Date("2000-07-31")
CHIRPS_PERIOD <- seq(from = as.Date(CHIRPS_START), to = as.Date(CHIRPS_END), by = 1)

i = 17
tmp_file <- paste0(PATH_CHIRPS, "/chirps-v2.0.", format(as.Date(CHIRPS_PERIOD[i]), "%Y.%m.%d"), ".tif")

tmp_raster <- raster(tmp_file)
tmp_raster[tmp_raster==-9999] <- NaN  # ocean cells
tmp_raster

Uganda_rec <- as(extent(29.5, 35.25, -1.5, 4.5), 'SpatialPolygons')
tmp_crop <- crop(tmp_raster, Uganda_rec)

crs(tmp_crop) <- "+proj=longlat +datum=WGS84 +no_defs"

# png("Uganda_CHIRPS_raster_20000717.jpeg", units="in", width=8, height=8, res=150)
# plot(tmp_crop)
# plot(Uganda_lev1, add = TRUE)
# dev.off()

df = data.frame(rasterToPoints(tmp_crop))
colnames(df) <- c("lon", "lat", "CHIRPS")
shapefile_df <- fortify(level_chosen)
df_norm <- df %>% filter(CHIRPS <=30) 
df_extreme <- df %>% filter(CHIRPS >30)
my.palette <- brewer.pal(n = 9, name = "YlGnBu")  # colour the map, area with highest precipitation is in deep blue

# Uganda_ver2_CHIRPS_20000715 <- 
  ggplot() + geom_raster(data=df_norm, aes(x=lon, y=lat, fill=CHIRPS)) +
  geom_raster(data=df_extreme, aes(x=lon, y=lat, fill=CHIRPS)) +
  scale_fill_gradientn(colours = my.palette, limits = c(0,30),
                        na.value = "transparent") +
  geom_tile(data=df_extreme, aes(x=lon, y=lat), fill="red") +
  geom_polygon(data = shapefile_df, aes(long, lat, group=group), colour = "black", fill=NA) 

ggsave("Uganda_ver2_CHIRPS_20000715.png", units="in", width=6, height=6, dpi=200)

# mymap <- rasterFromXYZ(tmp_crop)
breakpoints <- c(0,50)
colors <- c("YlGnBu","black")
plot(tmp_crop, ylim = c(0,50), col=colors)

plot(tmp_crop)
plot(Uganda_lev1, add = TRUE)


#### ========= FEWS/ARC2 ===========

ARC2_START  <- as.Date("2000-07-01")
ARC2_END    <- as.Date("2000-07-31")
ARC2_PERIOD <- seq(from = as.Date(ARC2_START), to = as.Date(ARC2_END), by = 1)

Uganda_rect <- data.frame(W = 29.5, E = 35.25, S = -1.5, N = 4.5)

i = 13
tmp_df <- arc2(date = ARC2_PERIOD[i])  # get the data from ftp of NOAA

# Crop only rectangle covering Uganda 
tmp_df_sel <- tmp_df %>% filter(lon >= Uganda_rect$W  & lon <= Uganda_rect$E & lat >= Uganda_rect$S & lat <= Uganda_rect$N)
tmp_raster <- rasterFromXYZ(tmp_df_sel)
crs(tmp_raster) <- "+proj=longlat +datum=WGS84 +no_defs"  # make sure raster data have the same projection reference
tmp_raster[tmp_raster==-999] <- NaN  # ocean or missing values
tmp_raster

tmp_crop <- tmp_raster

png("Uganda_ARC2_raster_20000717.jpeg", units="in", width=8, height=8, res=150)
plot(tmp_crop)
plot(Uganda_lev1, add = TRUE)
dev.off()


df = data.frame(rasterToPoints(tmp_crop))
colnames(df) <- c("lon", "lat", "ARC2")
shapefile_df <- fortify(level_chosen)
df_norm <- df %>% filter(ARC2 <=30) 
df_extreme <- df %>% filter(ARC2 >30)
my.palette <- brewer.pal(n = 9, name = "YlGnBu")  # colour the map, area with highest precipitation is in deep blue

Uganda_ver2_ARC2_20000715 <- ggplot() + geom_raster(data=df_norm, aes(x=lon, y=lat, fill=ARC2)) +
  geom_raster(data=df_extreme, aes(x=lon, y=lat, fill=ARC2)) +
  scale_fill_gradientn(colours = my.palette, limits = c(0,30),
                       na.value = "transparent") +
  geom_tile(data=df_extreme, aes(x=lon, y=lat), fill="red") +
  geom_polygon(data = shapefile_df, aes(long, lat, group=group), colour = "black", fill=NA) 

ggsave("Uganda_ver2_ARC2_20000715.png", units="in", width=6, height=6, dpi=200)
