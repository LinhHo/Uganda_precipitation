
rm(list=ls())

#######################################
library(ncdf4) 

dirw <- "/Users/user/Desktop/2T"

setwd(dirw)  


  
  #######################################
  # Read Land-Sea Mask
  #######################################
  fname_LSM <- paste("ERA5_Europe_LSM.nc", sep="")
  nc <- nc_open(fname_LSM, write=FALSE, readunlim=TRUE, verbose=FALSE)
  v1 <- nc$var[[1]]
  lon1 <- v1$dim[[1]]$vals
  lat1 <- v1$dim[[2]]$vals
  LSM <- ncvar_get( nc, v1)
  nc_close(nc)
  
  nx <- length(lon1)
  ny <- length(lat1)

  
  ###################################
  # Compute lat-lon dependent values
  ###################################    
  
  rad=6370.e0
  un_length=pi*rad/180.
  
  dydeg <- min(diff(lat1))
  if(abs(dydeg - max(diff(lat1))) > 1.e-4) stop ("irregular latitudes")
  dxdeg <- min(diff(lon1))
  if(abs(dxdeg - max(diff(lon1))) > 1.e-4) stop ("irregular longitudes")
  
  x_len <- dxdeg * un_length    
  y_len <- dydeg * un_length
  y_grid_cos_rad <- cos(lat1*pi/180.)
  y_weights <- y_len * y_grid_cos_rad
  
  area_xy <- array(0,c(nx,ny))
  
  for (i in 1:nx) { for (j in 1:ny) { area_xy[i,j] <- x_len*y_len*y_grid_cos_rad[j] } }
  

  
  ###################################     
  
  fname_fld <- paste("yearmean_t2m/masked_data/yearmean_fr_ecmwf_era5_analysis_S2000010100_E201712312300_1hr_EU1_025d_2m_t2m_noc_org.nc", sep="")
  nc <- nc_open(fname_fld, write=FALSE, readunlim=TRUE, verbose=FALSE)
  v1 <- nc$var[[1]]
  lon2 <- v1$dim[[1]]$vals
  lat2 <- v1$dim[[2]]$vals
  ntt <- v1$dim[[3]]$vals
  fld <- ncvar_get( nc, v1)
  nc_close(nc)

  nx2 <- length(lon2)
  ny2 <- length(lat2)  
  nt <- length(ntt)  
  
  ### got as far as this 28/09/18 ###
  
  # mask based on NUTS
  area_xy[which(is.na(fld[,,1]) == T)] <- NA
  LSM[which(is.na(fld[,,1]) == T)] <- NA
  area_xy_LSM <- area_xy
  area_xy_LSM <- area_xy *LSM
  
  # average with and without latitude dependence 
  fldmsk <- fld 
  fldmskl <- fld 
  for (n in seq(1,nt)) {
    fldmsk[,,n] <- fld[,,n] * LSM
    fldmskl[,,n] <- fld[,,n] * area_xy_LSM
  }

  fldm <- apply(fldmsk,3,sum,na.rm=T)/sum(LSM,na.rm=TRUE)
  fldml <- apply(fldmskl,3,sum,na.rm=T)/sum(area_xy_LSM,na.rm=TRUE)
  
  print(fldm)
  print(fldml)