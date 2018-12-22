# Jeff R practice
# You probably need to install.packages('data.table')
library(stringr)
library(dplyr)
library(data.table)

# Source me, then do:
# FOR PLAYBACK
# aziData <- processAziCsv('pathToMyAzi')
# difarData <- formatPlayback('pathToMyDifar')   #this is the DIFAR_single_buoy_308.csv file.
# finalData <- matchToDifar(aziData, difarData)
# FOR BLUE WHALES (rename these whatever you want)
# aziData <- processAziCsv('pathToBWAzi')
# difarData <- readRDS('pathToCCData')  # this is ccData.RData
# finalData <- matchToDifar(aziData, difarDat)

# DATA KEY
# CallTime - Time of the call, timezone is UTC
# NoiseBearing - median bearing of your Azi noise measurements. Matched by time.
# AziBearing - median bearing of your Azi signal measurements. Matched by time.
# AziFrequency - median frequency of your Azi signal measurements
# DifarBearing - Bearing from Difar box method
# RealBearing - Actual bearing the call should be
# ClipLength - length of call (size of box drawn from Difar method)
# DifarFrequency - frequency from Difar method
# SignalAmplitude - relative dB level of the Difar box
# StationNumber - Playback station number. You probably want to look at one station at a time
#   because the 'SignalAmplitude' is probably different at different stations
# Distance - Distance from buoy to boat in kilometers
# CallType - dn & up are FM sweep calls. Tones are tonal calls.
# Intensity - Sort of a measure of how loud the playback was. Number doesn't mean anything.
# DifarError - RealBearing - DifarBearing
# AziError - RealBearing - AziBearing
# Channel - Channel #
# Buoy/Boat Lat/Long - Coordinates of ship and buoy

processAziCsv <- function(file) {
  if(missing(file)) {
    file <- file.choose()
  }
  if(!grepl('csv', file)) {
    stop('I want a csv')
  }
  aziData <- read.csv(file, stringsAsFactors = FALSE)
  whichChan <- grepl('Channel', colnames(aziData))
  aziData <- aziData[,whichChan]
  chan <- gsub('Channel([0-9]*).*', '\\1', colnames(aziData))
  type <- gsub('.*_(.*)$', '\\1', colnames(aziData))
  result <- apply(aziData, 2, processAziColumn)
  for(i in seq_along(result)) {
    result[[i]]$Channel <- chan[i]
    result[[i]]$AziType <- type[i]
  }
  bind_rows(result) %>%
    mutate(Channel = as.integer(Channel))
}

processAziColumn <- function(aziCol) {
  aziCol <- str_trim(aziCol)
  aziCol <- aziCol[which(nchar(aziCol) > 0)]
  timeLocs <- grep('Absolute time', aziCol)

  start <- 1
  result <- list()
  # Check for consecutive timelocs (no data points)
  for(i in seq_along(timeLocs)) {
    if(i != 1 &&
       timeLocs[i] == (timeLocs[i-1]+1)) {
      start <- timeLocs[i] + 1
      next
    }
    thisTime <- gsub('.*time:\\s*(.*)$', '\\1', aziCol[timeLocs[i]])
    thisTime <- as.POSIXct(thisTime, format = '%d-%b-%Y %H:%M:%S', tz='UTC')
    tryCatch({thisDf <- processAziRow(aziCol[start:(timeLocs[i]-1)]) %>%
      mutate(AziTime = thisTime + AziTime,
             DetectionNumber = i)},
      error = function(e) {
        print(i)
        browser()
      })
    result[[i]] <- thisDf
    start <- timeLocs[i]+1
  }
  bind_rows(result)
}

processAziRow <- function(aziRow) {
  result <- list()
  splitAzi <- gsub('.*Point\\s([0-9]*).*Time:\\s([0-9\\.]*).*Frequency:\\s*([0-9\\.]*).*Directions:\\s*([0-9\\.]*).*',
                   '\\1-\\2-\\3-\\4', aziRow) %>% str_split('-', simplify=TRUE)
  result$Point <- as.integer(splitAzi[,1])
  result$AziTime <- as.numeric(splitAzi[,2])
  result$AziTime <- result$AziTime - min(result$AziTime)
  result$AziFrequency <- as.numeric(splitAzi[,3])
  result$AziBearing <- as.numeric(splitAzi[,4])
  bind_rows(result)
}

formatPlayback <- function(playbackCsv) {
  data <- read.csv(playbackCsv, stringsAsFactors = FALSE) %>%
    mutate(UTC = as.POSIXct(UTC, format = '%Y-%m-%d %H:%M:%S', tz='UTC'),
           Channel = sapply(Buoy, function(x) switch(x,
                                                     'NE' = 1, 'SW' = 2, 'SE' = 3, 'NW' = 4), USE.NAMES = FALSE)) %>%
    select(-X, -posixDate, -Buoy)
  data
}

matchToDifar <- function(aziData, difarData) {
  result <- list()
  aziData <- aziData %>%
    group_by(DetectionNumber, Channel, AziType) %>%
    summarise(AziTime = median(AziTime, na.rm = TRUE),
              AziFrequency = median(AziFrequency, na.rm=TRUE),
              AziBearing = median(AziBearing, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(UTC = AziTime)
  # Ours start at 1
  if('Buoy' %in% colnames(difarData)) {
    difarData$Channel <- as.numeric(difarData$Buoy)
  }
  if(0 %in% unique(difarData$Channel)) {
    difarData$Channel <- difarData$Channel + 1
  }
  difarData <- filter(difarData,
                      UTC < max(aziData$AziTime),
                      UTC > min(aziData$AziTime))

  for(i in unique(aziData$Channel)) {
    thisAziSignal <- filter(aziData, Channel==i, AziType == 'Signal') %>%
      select(AziTime, AziBearing, AziFrequency, UTC) %>% data.table(key = 'UTC')
    thisDifar <- data.table(filter(difarData, Channel==i), key='UTC')
    thisDifar <- thisAziSignal[thisDifar, roll='nearest']
    thisNoise <- filter(aziData, Channel==i, AziType=='Noise') %>%
      select(NoiseTime = AziTime, NoiseBearing = AziBearing, UTC) %>% data.table(key='UTC')
    thisDifar <- thisNoise[thisDifar, roll='nearest']
    result[[i]] <- thisDifar
  }
  result <- bind_rows(result) %>%
    mutate(TimeSep = abs(difftime(UTC, AziTime, units='secs'))) %>%
    filter(TimeSep < 60)
  # Playback case has RealBearing
  if('RealBearing' %in% colnames(result)) {
    result <- result %>%
    mutate(AziError = (RealBearing - AziBearing) %% 360,
           AziError = ifelse(AziError > 180, AziError - 360, AziError),
           Channel = Channel - 1) %>%
    select(CallTime = UTC, AziTime, NoiseBearing, AziBearing, AziFrequency, DifarBearing = DifarAdj,
           RealBearing, ClipLength, DifarFrequency, SignalAmplitude, StationNumber = TrackedGroup,
           Distance, CallType, Intensity, DifarError = AdjError, AziError, Channel,
           BuoyLat, BuoyLong, BoatLat, BoatLong)
  } else {
    warning('Removing 11.7 from each AziBearing because we forgot the first time around.')
    result %>%
      mutate(AziBearing = (AziBearing - 11.7) %% 360,
             NoiseBearing = (NoiseBearing - 11.7) %% 360,
             Channel = Channel - 1) %>%
      select(CallTime = UTC, AziTime, NoiseBearing, AziBearing, AziFrequency, DifarBearing = DIFARBearing,
             ClipLength, DifarFrequency, SignalAmplitude, Channel, Station=station, CallId = detection, calibrationValue)
  }
}
