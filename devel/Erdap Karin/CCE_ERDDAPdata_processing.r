#--------------------------------------------------------------------------------------
# CCE_ERDDAPdata_processing.r  -- reads ncdf files and processes CCE
#                                 data to create grid files
#
# Written by Karin Forney and Elizabeth Becker, 9/24/2014
# Last modified: 09/24/2014
#
#
#--------------------------------------------------------------------------------------
# Clear workspace 
#
  rm(list=ls())

#-------------------------------SET UP FUNCTIONS---------------------------------------
# Function to check for libararies
#
 is.installed <- function(x){
    is.element(x, installed.packages()[,1])
  } 
#--------------------------------------------------------------------------------------
# To be written once we've figured out how... (if they are useful as functions)
#
#  fn.9pixelSD
#--------------------------------------------------------------------------------------
#  fn.timeavg
#--------------------------------------------------------------------------------------
#  fn.getpoint
#--------------------------------------------------------------------------------------
#  fn.getmean
#--------------------------------------------------------------------------------------
#
# If needed install and load ncdf5 package
#
  if (!is.installed("ncdf4")){
    install.packages("ncdf4")
  }
  library(ncdf4)

#
# Initialize variables and open grid pixel file
#  
  whichgrid <- 3                      #Choose index for grid: 1=5km,2=10km,3=25km
  gridsize = c('05km','10km','25km')  # provide gridsize label for output file
  avgbox = c(0.06, 0.12, 0.30)        # size of averaging box in degrees
  gridfile <- c('Grid_Lat30to50_Lon225to245_Step0.045_5km.csv',
                'Grid_Lat30to50_Lon225to245_Step0.090_10km.csv',
                'Grid_Lat30to50_Lon225to245_Step0.225_25km.csv')

  
  grid.path <- 'C:/Users/EABECKER/Documents/R_chopping_code/Prediction_Grids/Grids and Mastersegs/'
  nc.path <- 'C:/Users/EABECKER/Documents/R_xtracto/ERDDAP_Data/'
  out.path <- 'C:/Users/EABECKER/Documents/HabModels_CCE_2013/Datasets/EAB_CCE/CCE_Grid_Pred_Data/ERDDAP_grids/'

  grid.pixelfile <- paste(grid.path,gridfile[whichgrid],sep="")
  grid.pixels    <- read.csv(grid.pixelfile, header=TRUE)
  num.pixels     <- nrow(grid.pixels)
  grid.data      <- grid.pixels
  gridlon        <- grid.data$lon360
  gridlat        <- grid.data$lat

  periodlen <- 8                      # days in each period
  datayears <- c('1991','1993','1996','2001','2005','2008','2009')
  beg8day <- '-06-29'                 #First Day1 of annual 8-day windows
  end8day <- '-11-28'                 #Last  Day1 of annual 8-day windows

  t1<-Sys.time()
  print(t1)

  for(y in 1:length(datayears)) {               # Loop through each year 
    yr <- datayears[y]                          
    YearDay1 <-as.Date(paste(yr, beg8day,sep=""))
    YearDay2 <-as.Date(paste(yr, end8day,sep=""))
    nper <- (YearDay2 - YearDay1)/periodlen + 1
    for (p in 1:nper) {
      BegDay <- YearDay1 + (p-1)*8 

#
#  For each data type, open file the appropriate nc file
#  and process data as needed  
#
#  REYNOLDS SST -----------------------------------------------------------------------
#
      datafile <- paste(nc.path,'CCE_sst_ncdcOisst2Agg_',yr,'.nc',sep="")
      varname <- 'sst'
      nc.data <- nc_open(datafile)
  
      lat <- ncvar_get(nc.data,'latitude')
      lon <- ncvar_get(nc.data,'longitude')
      tim <- ncvar_get(nc.data,'time')
      day <- substr(as.POSIXlt(tim,origin='1970-01-01',tz= "GMT"),1,10)
      data.var <-  ncvar_get(nc.data,varname)
      nrows <- length(lon)
      ncols <- length(lat)
#
# Find starting date in file; check to make sure it is in file
#
      index1 <- which(day==BegDay)
      if (length(index1)==0) print('Not OK! Missing dates in nc file.')
#  
# Calculate average and SD of SST for 'numdays' days, beginning on BegDay 
#
      numdays <- 8
      index2 <- index1 + numdays-1
      days <-array(data.var[,,index1:index2],dim=c(nrows,ncols,numdays))
      SSTre.mean   <- matrix(apply(days, c(1,2), mean, na.rm=TRUE), 
                             nrows, ncols, dimnames=list(lon,lat))
      SSTre.SDtime <- matrix(apply(days, c(1,2), sd,   na.rm=TRUE), 
                             nrows, ncols, dimnames=list(lon,lat))
#
# Get data for mid-point of 8-day window (Day 5) to calculate daily SST.SDspace for
# up to 9 pixels around each data cell.

      Day5 <- BegDay+4
      midpt <- which(day==Day5)
      if (length(midpt)==0) print('Not OK! Missing dates in nc file.')
      Day5data <- matrix(data.var[,,midpt], nrows, ncols, dimnames=list(lon,lat))
      SSTre.SDspace <- Day5data
      SSTre.SDspace[,] <- NA

      for (r in 1:nrows){
        for (c in 1:ncols) {    
          row1 <- max(r-1,1)
          row2 <- min(r+1,nrows)
          col1 <- max(c-1,1)
          col2 <- min(c+1,ncols)
          SSTre.SDspace[r,c] <- sd(Day5data[c(row1:row2),c(col1:col2)], na.rm=TRUE)
        }
      }

# Add columns for new SSTre variables to grid data
#
      grid.data$SSTre.mean   <- NA
      grid.data$SSTre.SDtime <- NA
      grid.data$SSTre.SDspace <- NA

#
# Find data closest to each grid point and add to data.frame
#
      for (i in 1:num.pixels) {
        lat.indx<-which(abs(lat-gridlat[i])==min(abs(lat-gridlat[i])))
        lon.indx<-which(abs(lon-gridlon[i])==min(abs(lon-gridlon[i])))
        nearest.cells <- length(lat.indx) * length(lon.indx)
        num.vals <- which(!is.na(SSTre.mean[lon.indx,lat.indx]))
        if (length(num.vals)>0) {
          if ((length(num.vals)==nearest.cells) | (nearest.cells == 1)) {
            grid.data$SSTre.mean[i]    <- SSTre.mean[lon.indx[1],lat.indx[1]] 
            grid.data$SSTre.SDtime[i]  <- SSTre.SDtime[lon.indx[1],lat.indx[1]] 
            grid.data$SSTre.SDspace[i] <- SSTre.SDspace[lon.indx[1],lat.indx[1]] 
          } else {
            grid.data$SSTre.mean[i]    <- SSTre.mean[lon.indx,lat.indx][num.vals[1]]
            grid.data$SSTre.SDtime[i]  <- SSTre.SDspace[lon.indx,lat.indx][num.vals[1]]
            grid.data$SSTre.SDspace[i] <- SSTre.SDtime[lon.indx,lat.indx][num.vals[1]]
          }
        }
      }

  nc_close(nc.data)

#
#  NEXT VARIABLE ----------------------------------------------------------------------
#
#  [later...]

#
#--------------------------------------------------------------------------------------
#
# Print data to csv

      grid.datafile <- paste(out.path,'CCE_',gridsize[whichgrid],'_',BegDay,'.csv',sep="")
      write.table(grid.data, grid.datafile, sep = "," , col.names = TRUE, row.names = FALSE)

    }  # Get next 8-day period for this year

  }  # Next year

   t2<-Sys.time()
   print(t2-t1)


