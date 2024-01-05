# Uganda precipitation analysis
# Linh Ho (WEMC) 20/03/2019
# Import shapefile of Uganda - level 1 (district)

# install.packages("rgdal")  # only for the first time
library(rgdal)
library(ggplot2)
library(mapproj)

baseDir <- "D:/WEMC/Uganda_precipitation"
setwd(baseDir)
PATH_DATA <- file.path(baseDir, "DATA")
PATH_FIGURE <- file.path(baseDir, "FIGURE")
PATH_shapefile <- file.path(PATH_DATA, "Shapefiles")

# read shapefile using rgdal package
require(rgdal)

# coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
PATHs <- file.path(PATH_shapefile, "uga_admbnda_adm1_ubos_v2")
Uganda_lev1 <- readOGR(dsn = PATHs, layer = "uga_admbnda_adm1_UBOS_v2")  # read the shapefile with the extension part .shp

# coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
PATHs <- file.path(PATH_shapefile, "uga_admbnda_adm3_ubos_v5")
Uganda_lev3 <- readOGR(dsn = PATHs, layer = "uga_admbnda_adm3_UBOS_v5")  # read the shapefile with the extension part .shp

# # coord. ref. : +proj=utm +zone=36 +south +ellps=clrk80 +towgs84=-157.0,-2.0,-299.0,0.0,0.0,0.0,0.0 +units=m +no_defs 
# PATHs <- file.path(PATH_shapefile, "uganda_parishes_cleaned_attached")
# Uganda_parish <- readOGR(dsn = PATHs, layer = "uganda_parishes_cleaned_attached")  # read the shapefile with the extension part .shp

# convert the shapefile to a dataframe for use in ggplot2
Uganda_lev1_df <- fortify(Uganda_lev1)
Uganda_lev3_df <- fortify(Uganda_lev3)
Uganda_parish_df <- fortify(Uganda_parish)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
map1 <- ggplot() + geom_path(data = Uganda_lev1_df, 
                  aes(x = long, y = lat, group = group),
                  color = 'gray', fill = 'white', size = .2)
map3 <- ggplot() + geom_path(data = Uganda_lev3_df, 
                             aes(x = long, y = lat, group = group),
                             color = 'gray', fill = 'white', size = .2)
map_parish <- ggplot() + geom_path(data = Uganda_parish_df, 
                             aes(x = long, y = lat, group = group),
                             color = 'gray', fill = 'white', size = .2)


print(map1)
print(map3)
print(map_parish)

# Using the ggplot2 function coord_map will make things look better and it will also let you change
# the projection. But sometimes with large shapefiles it makes everything blow up.
map_projected <- map3 +  coord_map()

print(map_projected)
