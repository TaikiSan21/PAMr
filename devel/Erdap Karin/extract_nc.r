#--------------------------------------------------------------------------
# extract_nc.r    -- reads ncdf files for processing
#
# Written by Karin Forney and Elizabeth Becker, 9/24/2014
# Last modified: 
#
# Code tested on small data set downloaded from ERDDAP and saved separately 
# as a 'nc' file (CCE_test_sst_ncdcOisst2Agg_2005.nc) and as a 'csv' file 
# (CCE_test_sst_ncdcOisst2Agg_2005.csv) for manual verification of 
# calculations.
#
#--------------------------------------------------------------------------
# Clear workspace 
#
  rm(list=ls())
#
#  Set up function to check for libararies
#
 is.installed <- function(x){
    is.element(x, installed.packages()[,1])
  } 

#
# Clear workspace and, if needed install packages and load library
#
  if (!is.installed("ncdf4")){
    install.packages("ncdf4")
  }
  library(ncdf4)

#
#  Select and open file as nc.data  
#
  filename <- "CCE_test_sst_ncdcOisst2Agg_2005.nc"
  nc.data <- nc_open(filename)
  str(nc.data)

  lat <- ncvar_get(nc.data,'latitude')
  lon <- ncvar_get(nc.data,'longitude')
  period <- ncvar_get(nc.data,' ncvar_get(nc.data,'latitude')time')
  day <-substr(as.POSIXlt(period,origin='1970-01-01',tz= "GMT"),1,10)

  data.var <-  ncvar_get(nc.data,'sst')
  data.var[1:10]
  length(data.var)
  str(data.var)

#  day1<-matrix(data.var[,,1], length(lon), length(lat), dimnames=list(lon,lat))
 
  numdays <- 8
  days<-array(data.var[,,1:numdays],dim=c(length(lon),length(lat),numdays))
  meandays <- apply(days, c(1,2), mean)
  matrix(meandays,  length(lon), length(lat), dimnames=list(lon,lat))

  nc_close(nc.data)



