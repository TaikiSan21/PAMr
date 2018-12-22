# GetDatabyURL.r  ------------------------------------------------------
#
#  Calling script to obtain CCE grid data
#
#  Written by Karin Forney  9/22/2014
#  Last modified:  09/23/2014
#
# ----------------------------------------------------------------------
# Clear workspace
  rm(list=ls())
#
# Define functions (taken from xtractomatic_erddap_v3.2.7.R, 23 Sep 2014
# -------------------------------------------------------------------------
  geturl<-function(myURL,destfile,cacheOK=TRUE,mode="wb",quiet=FALSE){
    numtries<-2
    tryn<-1

    while(tryn<numtries){
      try(downloadReturn<-download.file(myURL,destfile=destfile,cacheOK=TRUE,mode="wb",quiet=quiet))
      if(!exists('downloadReturn')){downloadReturn<-1}
      if(downloadReturn != 0){
        print(myURL)
        print(paste("Tried ",tryn," of ",numtries," times",sep=''))
        print("There was an error in the url call.  See message on screen and URL called")
        tryn<-tryn+1    
        Sys.sleep(20)
      } else {
        tryn<-numtries
      }
 
    } # end of while
    return(downloadReturn)
  } # end of function
# ---------------------------------------------------------------------------
#
  is.installed <- function(x){
    is.element(x, installed.packages()[,1])
  } 
# ---------------------------------------------------------------------------
#
# Check for and if needed, install required packages
#
  if (!is.installed("ncdf4")){
    install.packages("ncdf4")
  }
  if (!is.installed("sp")){
    install.packages("sp")
  }
  library(sp)
  library(ncdf4)
#
# ===========================================================================
#  Set ERDDAP URL and filetype; define study area boundaries and years
#
  urlbase<-'http://coastwatch.pfeg.noaa.gov/erddap/griddap/'
  filetype <- 'nc'        #e.g. 'nc' or 'csv'

  ymin <- 30              #30
  ymax <- 50              #50
  xmin <- 225             #225
  xmax <- 245             #245

  datayears <- c('1991','1993','1996','2001','2005','2008','2009')
#  datayears <- '2005'

#......................................................................
# Satellite data and variable names:
#
# Name             datasetname      varname       resolution
#  Reynolds SST     ncdcOisst2Agg      sst       25km, 0.225deg
#  Pathfinder SST   erdPHssta7day      sst        5km, 0.045deg
#                    
#
#-------------------------------------------------------------------------DONE
# SST: Reynolds Optimal Interpolation Final+Prelim (AVHRR only & in situ)
# Daily, 25km - get value for each day in time range
#
  datasetname   <- 'ncdcOisst2Agg'
  varname       <- 'sst'
  day.interval  <- 1
  day1 <- '-06-29T00:00:00Z'
  day2 <- '-12-06T00:00:00Z'


  t1<-Sys.time()
  print(t1)

  for(y in 1:length(datayears)) {               # Loop through each year 
    yr <- datayears[y]                          
    time1 <-paste(yr,day1,sep="")
    time2 <-paste(yr,day2,sep="")

    outfile <- paste('CCE_',varname,'_',datasetname,
                     '_',yr,'.',filetype,sep="")

    myURL=paste(urlbase,datasetname,'.',filetype,'?',varname,
         '[(',time1,'):',day.interval,':(',time2,')]',
         '[(0.0):1:(0.0)]',
         '[(',ymin,'):1:(',ymax,')]',
         '[(',xmin,'):1:(',xmax,')]',sep="")

    print(paste('Creating ',outfile,'...',sep=""))

    geturl(myURL,outfile)
    }

    t2<-Sys.time()
    print(t2-t1)
#
#-------------------------------------------------------------------------DONE
# SST: Pathfinder SST er 5.0, Day and Night, Global, Science Quality
# 7-day composite, 5km - get value closest to day 5 of 8-day periods
#
  datasetname   <- 'erdPHssta7day'
  varname       <- 'sst'
  day.interval  <- 8
  day1 <- '-07-03T00:00:00Z'
  day2 <- '-12-02T00:00:00Z'

  t1<-Sys.time()
  print(t1)

  for(y in 1:length(datayears)) {               # Loop through each year 
    yr <- datayears[y]                          
    time1 <-paste(yr,day1,sep="")
    time2 <-paste(yr,day2,sep="")

    outfile <- paste('CCE_',varname,'_',datasetname,
                     '_',yr,'.',filetype,sep="")

    myURL=paste(urlbase,datasetname,'.',filetype,'?',varname,
         '[(',time1,'):',day.interval,':(',time2,')]',
         '[(0.0):1:(0.0)]',
         '[(',ymin,'):1:(',ymax,')]',
         '[(',xmin,'):1:(',xmax,')]',sep="")

    print(paste('Creating ',outfile,'...',sep=""))

    geturl(myURL,outfile)
    }

    t2<-Sys.time()
    print(t2-t1)
#
#-------------------------------------------------------------------------DONE
# Chlorophyll-a, OrbView-2 SeaWiFS, Global 
#  -day composite, 0.08deg - get value closest to day 5 of 8-day periods
#
  datasetname   <- 'erdSWchla8day'
  varname       <- 'chlorophyll'
  day.interval  <- 8
  day1 <- '-07-03T00:00:00Z'
  day2 <- '-12-02T00:00:00Z'

  t1<-Sys.time()
  print(t1)

  for(y in 1:length(datayears)) {               # Loop through each year 
    yr <- datayears[y]                          
    time1 <-paste(yr,day1,sep="")
    time2 <-paste(yr,day2,sep="")

    outfile <- paste('CCE_',varname,'_',datasetname,
                     '_',yr,'.',filetype,sep="")

    myURL=paste(urlbase,datasetname,'.',filetype,'?',varname,
         '[(',time1,'):',day.interval,':(',time2,')]',
         '[(0.0):1:(0.0)]',
         '[(',ymin,'):1:(',ymax,')]',
         '[(',xmin,'):1:(',xmax,')]',sep="")

    print(paste('Creating ',outfile,'...',sep=""))

    geturl(myURL,outfile)
    }

    t2<-Sys.time()
    print(t2-t1)

# -------------------------------------------------------------------- DONE
# Chlorophyll-a Aqua MODIS, NPP Pacific Ocean
#  8-day composite, 0.025g - get value closest to day 5 of 8-day periods
#
  datasetname   <- 'erdMBchla8day'
  varname       <- 'chlorophyll'
  day.interval  <- 8
  day1 <- '-07-03T00:00:00Z'
  day2 <- '-12-02T00:00:00Z'

  t1<-Sys.time()
  print(t1)

  for(y in 1:length(datayears)) {               # Loop through each year 
    yr <- datayears[y]                          
    time1 <-paste(yr,day1,sep="")
    time2 <-paste(yr,day2,sep="")

    outfile <- paste('CCE_',varname,'_',datasetname,
                     '_',yr,'.',filetype,sep="")

    myURL=paste(urlbase,datasetname,'.',filetype,'?',varname,
         '[(',time1,'):',day.interval,':(',time2,')]',
         '[(0.0):1:(0.0)]',
         '[(',ymin,'):1:(',ymax,')]',
         '[(',xmin,'):1:(',xmax,')]',sep="")

    print(paste('Creating ',outfile,'...',sep=""))

    geturl(myURL,outfile)
    }

    t2<-Sys.time()
    print(t2-t1)

#-------------------------------------------------------------------------DONE, but check 2005 & 2008
# Primary Productivity, Aqua MODIS & Pathfinder PP, Global, Experimental
#  8-day composite, 0.04deg - get value closest to day 5 of 8-day periods
#
  datasetname   <- 'erdPPbfp28day'
  varname       <- 'productivity'
  day.interval  <- 8
  day1 <- '-07-03T00:00:00Z'
  day2 <- '-12-02T00:00:00Z'

  t1<-Sys.time()
  print(t1)

  for(y in 1:length(datayears)) {               # Loop through each year 
    yr <- datayears[y]                          
    time1 <-paste(yr,day1,sep="")
    time2 <-paste(yr,day2,sep="")

    outfile <- paste('CCE_',varname,'_',datasetname,
                     '_',yr,'.',filetype,sep="")

    myURL=paste(urlbase,datasetname,'.',filetype,'?',varname,
         '[(',time1,'):',day.interval,':(',time2,')]',
         '[(0.0):1:(0.0)]',
         '[(',ymin,'):1:(',ymax,')]',
         '[(',xmin,'):1:(',xmax,')]',sep="")

    print(paste('Creating ',outfile,'...',sep=""))

    geturl(myURL,outfile)
    }

    t2<-Sys.time()
    print(t2-t1)


#------------------------------------------------------------------------- DONE
# AVISO SSH Science Quality - for SSH Deviation
#  1-day, 0.25deg - get value for each day in time range
#
  datasetname   <- 'erdTAssh1day'
  varname       <- 'sshd'
  day.interval  <- 1
  day1 <- '-06-29T00:00:00Z'
  day2 <- '-12-06T00:00:00Z'

  t1<-Sys.time()
  print(t1)

  for(y in 1:length(datayears)) {               # Loop through each year 
    yr <- datayears[y]                          
    time1 <-paste(yr,day1,sep="")
    time2 <-paste(yr,day2,sep="")

    outfile <- paste('CCE_',varname,'_',datasetname,
                     '_',yr,'.',filetype,sep="")

    myURL=paste(urlbase,datasetname,'.',filetype,'?',varname,
         '[(',time1,'):',day.interval,':(',time2,')]',
         '[(0.0):1:(0.0)]',
         '[(',ymin,'):1:(',ymax,')]',
         '[(',xmin,'):1:(',xmax,')]',sep="")

    print(paste('Creating ',outfile,'...',sep=""))

    geturl(myURL,outfile)
    }

    t2<-Sys.time()
    print(t2-t1)

#------------------------------------------------------------------------- DONE
# AVISO geostrophic currents U-current - for EKE calculation
#  1-day, 0.25deg - get value for each day in time range
#
  datasetname   <- 'erdTAgeo1day'
  varname       <- 'u_current'
  day.interval  <- 1
  day1 <- '-06-29T00:00:00Z'
  day2 <- '-12-06T00:00:00Z'

  t1<-Sys.time()
  print(t1)

  for(y in 1:length(datayears)) {               # Loop through each year 
    yr <- datayears[y]                          
    time1 <-paste(yr,day1,sep="")
    time2 <-paste(yr,day2,sep="")

    outfile <- paste('CCE_',varname,'_',datasetname,
                     '_',yr,'.',filetype,sep="")

    myURL=paste(urlbase,datasetname,'.',filetype,'?',varname,
         '[(',time1,'):',day.interval,':(',time2,')]',
         '[(0.0):1:(0.0)]',
         '[(',ymin,'):1:(',ymax,')]',
         '[(',xmin,'):1:(',xmax,')]',sep="")

    print(paste('Creating ',outfile,'...',sep=""))

    geturl(myURL,outfile)
    }

    t2<-Sys.time()
    print(t2-t1)

#------------------------------------------------------------------------- DONE
# AVISO geostrophic currents U-current - for EKE calculation
#  1-day, 0.25deg - get value for each day in time range
#
  datasetname   <- 'erdTAgeo1day'
  varname       <- 'v_current'
  day.interval  <- 1
  day1 <- '-06-29T00:00:00Z'
  day2 <- '-12-06T00:00:00Z'

  t1<-Sys.time()
  print(t1)

  for(y in 1:length(datayears)) {               # Loop through each year 
    yr <- datayears[y]                          
    time1 <-paste(yr,day1,sep="")
    time2 <-paste(yr,day2,sep="")

    outfile <- paste('CCE_',varname,'_',datasetname,
                     '_',yr,'.',filetype,sep="")

    myURL=paste(urlbase,datasetname,'.',filetype,'?',varname,
         '[(',time1,'):',day.interval,':(',time2,')]',
         '[(0.0):1:(0.0)]',
         '[(',ymin,'):1:(',ymax,')]',
         '[(',xmin,'):1:(',xmax,')]',sep="")

    print(paste('Creating ',outfile,'...',sep=""))

    geturl(myURL,outfile)
    }

    t2<-Sys.time()
    print(t2-t1)
#------------------------------------------------------------------------- DONE
# Wind Stress, Quickscat, Global, Science Quality (8 Day Composite)
#  8-day composite, 0.125deg - get value closest to day 5 of 8-day periods
#
  datasetname   <- 'erdQSstress8day'
  varname       <- 'upwelling'
  day.interval  <- 8
  day1 <- '-07-03T00:00:00Z'
  day2 <- '-12-02T00:00:00Z'

  t1<-Sys.time()
  print(t1)

  for(y in 1:length(datayears)) {               # Loop through each year 
    yr <- datayears[y]                          
    time1 <-paste(yr,day1,sep="")
    time2 <-paste(yr,day2,sep="")

    outfile <- paste('CCE_',varname,'_',datasetname,
                     '_',yr,'.',filetype,sep="")

    myURL=paste(urlbase,datasetname,'.',filetype,'?',varname,
         '[(',time1,'):',day.interval,':(',time2,')]',
         '[(0.0):1:(0.0)]',
         '[(',ymin,'):1:(',ymax,')]',
         '[(',xmin,'):1:(',xmax,')]',sep="")

    print(paste('Creating ',outfile,'...',sep=""))

    geturl(myURL,outfile)
    }

    t2<-Sys.time()
    print(t2-t1)
#------------------------------------------------------------------------- DONE
# Primary Productivity, SeaWiFS and Pathfinder, Global, Experimental)
#  8-day composite, 0.08deg - get value closest to day 5 of 8-day periods
#
  datasetname   <- 'erdPPbfp18day'
  varname       <- 'productivity'
  day.interval  <- 8
  day1 <- '-07-03T00:00:00Z'
  day2 <- '-12-02T00:00:00Z'

  t1<-Sys.time()
  print(t1)

  for(y in 1:length(datayears)) {               # Loop through each year 
    yr <- datayears[y]                          
    time1 <-paste(yr,day1,sep="")
    time2 <-paste(yr,day2,sep="")

    outfile <- paste('CCE_',varname,'_',datasetname,
                     '_',yr,'.',filetype,sep="")

    myURL=paste(urlbase,datasetname,'.',filetype,'?',varname,
         '[(',time1,'):',day.interval,':(',time2,')]',
         '[(0.0):1:(0.0)]',
         '[(',ymin,'):1:(',ymax,')]',
         '[(',xmin,'):1:(',xmax,')]',sep="")

    print(paste('Creating ',outfile,'...',sep=""))

    geturl(myURL,outfile)
    }

    t2<-Sys.time()
    print(t2-t1)

#-------------------------------------------------------------------------


#-------------------------------------------------------------------------DONE
# GET ADDITIONAL JUNE DATA FOR 2005 FOR SEGMENT DATA EXTRACTIONS
# SST: Reynolds Optimal Interpolation Final+Prelim (AVHRR only & in situ)
# Daily, 25km - get value for each day in time range
#
  datasetname   <- 'ncdcOisst2Agg'
  varname       <- 'sst'
  day.interval  <- 1
  datayears <- '2005'
  day1 <- '-06-01T00:00:00Z'
  day2 <- '-12-06T00:00:00Z'


  t1<-Sys.time()
  print(t1)

  for(y in 1:length(datayears)) {               # Loop through each year 
    yr <- datayears[y]                          
    time1 <-paste(yr,day1,sep="")
    time2 <-paste(yr,day2,sep="")

    outfile <- paste('CCE_',varname,'_',datasetname,
                     '_',yr,'.',filetype,sep="")

    myURL=paste(urlbase,datasetname,'.',filetype,'?',varname,
         '[(',time1,'):',day.interval,':(',time2,')]',
         '[(0.0):1:(0.0)]',
         '[(',ymin,'):1:(',ymax,')]',
         '[(',xmin,'):1:(',xmax,')]',sep="")

    print(paste('Creating ',outfile,'...',sep=""))

    geturl(myURL,outfile)
    }

    t2<-Sys.time()
    print(t2-t1)
#


#-------------------------------------------------------------------------DONE
# Download small data subset in CSV and NC formats for testing of re-formatting
# for processing. 

  filetype <- 'csv'        #e.g. 'nc' or 'csv'

  ymin <- 36              #30
  ymax <- 38              #50
  xmin <- 234             #225
  xmax <- 239             #245

# SST: Reynolds Optimal Interpolation Final+Prelim (AVHRR only & in situ)
# Daily, 25km - get value for each day in time range
#
  datasetname   <- 'ncdcOisst2Agg'
  varname       <- 'sst'
  day.interval  <- 1
  datayears <- '2005'
  day1 <- '-06-01T00:00:00Z'
  day2 <- '-07-31T00:00:00Z'


  t1<-Sys.time()
  print(t1)

  for(y in 1:length(datayears)) {               # Loop through each year 
    yr <- datayears[y]                          
    time1 <-paste(yr,day1,sep="")
    time2 <-paste(yr,day2,sep="")

    outfile <- paste('CCE_test_',varname,'_',datasetname,
                     '_',yr,'.',filetype,sep="")

    myURL=paste(urlbase,datasetname,'.',filetype,'?',varname,
         '[(',time1,'):',day.interval,':(',time2,')]',
         '[(0.0):1:(0.0)]',
         '[(',ymin,'):1:(',ymax,')]',
         '[(',xmin,'):1:(',xmax,')]',sep="")

    print(paste('Creating ',outfile,'...',sep=""))

    geturl(myURL,outfile)
    }

    t2<-Sys.time()
    print(t2-t1)
#






