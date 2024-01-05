# Uganda precipitation
# Linh Ho - WEMC (20/03/2019)

# Read netCDF file - code from Alberto (19/03/2019)

##############

rm(list = ls())
library(zoo)
library(reshape2)
library(chron)
library(seas)
library(lubridate)
library(tidyverse)
library(ncdf4) 
select <- dplyr::select # avoid conflict with 'select' function from other packages

baseDir <- "D:/WEMC/Uganda_precipitation/"
setwd(baseDir)

PATH_DATA <- paste0(baseDir, "DATA")
PATH_FIGURE <- paste0(baseDir, "FIGURE")

PATHs <- paste0(PATH_DATA, "/ERA5_TP_025d_Uganda_01h_1980_2018")
YEARs <- c(1980:2018)

for (YEAR in YEARs) {
  file_name = paste0(PATHs, '/ERA5_TP_025d_Uganda_01h_', YEAR,  '.nc')
  
  nc <- nc_open(file_name, write=FALSE, readunlim=TRUE, verbose=FALSE)
  v1 <- nc$var[[1]]
  lon1 <- v1$dim[[1]]$vals
  lat1 <- v1$dim[[2]]$vals
  time1 <- v1$dim[[3]]$vals
  data1 <- ncvar_get( nc, v1)
  
  # # example for variable 2, then n
  # v2 <- nc$var[[2]]
  # data2 <- ncvar_get( nc, v2)
  
  nc_close(nc)
  
  # you may not need this
  tindex1 <- as.POSIXct(as.numeric(time1)*3600, origin = paste("1900-01-01", sep=""),tz="UTC" )
  
}


##############

# Batch command for R, you get the diagnostics/output 
# 
#     R CMD BATCH filename.R filename.ctr


    