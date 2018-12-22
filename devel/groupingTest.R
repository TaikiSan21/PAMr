library(dplyr)
library(RSQLite)
library(stringr)
library(data.table)
library(PamBinaries)

# Returns single AcEv, not list of them like other
getAllDetections <- function(binaryList, binFuns, sampleRate) {
  if(missing(sampleRate)) {
    sampleRate <- readline(prompt =
                     paste0('What is the sample rate for this data? ',
                            '(When processing all binaries, sample rate must be the same) '))
    sampleRate <- as.numeric(sampleRate)
  }
  calibrationUsed <- 'None'
  cat('Processing binary files... \n')
  pb <- txtProgressBar(min=0, max=length(binaryList), style=3)

  binData <- lapply(binaryList, function(bin) {
    thisBin <- loadPamguardBinaryFile(bin)
    for(i in seq_along(thisBin$data)) {
      thisBin$data[[i]]$sampleRate <- sampleRate
    }
    thisBinData <- calculateModuleData(thisBin, binFuns)
    thisBinData$BinaryFile <- basename(bin)
    setTxtProgressBar(pb, value=which(binaryList==bin))
    thisBinData
  })
  binData <- binData[sapply(binData, function(x) !is.null(x))]
  binData <- lapply(binData, function(x) split(x, x$detectorName))
  binData <- unlist(binData, recursive = FALSE)
  binData <- squishList(binData)

  # Should this function store the event ID? Right now its just the name
  # in the list, but is this reliable? Probably not

  acousticEvents <- AcousticEvent(detectors = binData, settings = DataSettings(sampleRate = sampleRate),
                  files = list(binaries=basename(binList), database='None', calibration=calibrationUsed))
  acousticEvents
}

getDetectionsFromDb <- function(dbFile, binaryList, binFuns, grouping=c('event', 'detGroup')) {
  dbData <- getDbData(dbFile, grouping)
  thisSr <- unique(dbData$sampleRate)
  if(length(thisSr) > 1) {
    warning('More than 1 sample rate found in database ',
            basename(dbFile),'.')
  }
  thisSource <- unique(dbData$SystemType)
  dbData <- select(dbData, -SystemType)
  calibrationUsed <- 'None'

  dbData <- lapply(
    split(dbData, dbData$BinaryFile), function(db) {
      thisBin <- getBinaryData(db, binaryList)
      if(length(thisBin)==0) {
        warning('Could not find the matching binary file for ', db$BinaryFile[1],
                ' in database ', basename(dbFile))
        return(NULL)
      }
      binData <- calculateModuleData(thisBin, binFuns)
      left_join(db, binData, by='UID') %>% distinct()
    })

  # This is a list for each binary, we want for each detector
  dbData <- dbData[sapply(dbData, function(x) !is.null(x))]

  dbData <- lapply(dbData, function(x) split(x, x$detectorName))
  names(dbData) <- NULL
  dbData <- unlist(dbData, recursive = FALSE)
  dbData <- squishList(dbData)

  # Split into events, then swap from Detector(Events) to Event(Detectors)
  # .names necessary to make sure we have all event numbers
  dbData <- purrr::transpose(
    lapply(dbData, function(x) split(x, x$parentUID)),
    .names = unique(unlist(sapply(dbData, function(x) x$parentUID)))
  )

  # Should this function store the event ID? Right now its just the name
  # in the list, but is this reliable? Probably not

  acousticEvents <- lapply(dbData, function(ev) {
    ev <- ev[sapply(ev, function(x) !is.null(x))]
    binariesUsed <- sapply(ev, function(x) unique(x$BinaryFile)) %>%
      unlist(recursive = FALSE) %>% unique()
    AcousticEvent(detectors = ev, settings = DataSettings(sampleRate = thisSr, soundSource=thisSource),
                  files = list(binaries=binariesUsed, database=basename(dbFile), calibration=calibrationUsed))
  })
  acousticEvents
}

getDbData <- function(dbFile, grouping=c('event', 'detGroup')) {
  # Combine all click/event tables, even by diff detector. Binary will have det name
  con <- dbConnect(SQLite(), dbFile)
  tables <- dbListTables(con)
  # Read in event data from either offlineclicks/events or detection
  # group localiser. Click version has common naming convention,
  # det group does not so we have to go look it up. If we are just
  # reading in all the data we only care about SA data
  switch(match.arg(grouping),
         'event' = {
           detTables <- grep('OfflineClicks', tables, value=TRUE)
           eventTables <- grep('OfflineEvents', tables, value=TRUE)
         },
         'detGroup' = {
           modules <- dbReadTable(con, 'PamguardModules')
           dgTables <- modules %>%
             mutate(Module_Name=str_trim(Module_Name),
                    Module_Type=str_trim(Module_Type)) %>%
             filter(Module_Name == 'Detection Group Localiser') %>%
             distinct(Module_Type, Module_Name)
           dgNames <- gsub(' ',  '_', dgTables$Module_Type)
           detTables <- sapply(dgNames, function(x) grep(x, tables, value=TRUE))
           eventTables <- detTables[!grepl('Children', detTables)]
           detTables <- detTables[grepl('Children', detTables)]
         },
         {
           dbDisconnect(con)
           stop("I don't know how to group by ", grouping, '.\n')
         }
  )

  if(length(detTables)==0 ||
     length(eventTables)==0) {
    stop('Could not find tables for grouping method "', grouping,
         '" in database ', basename(dbFile))
  }
  allDetections <- bind_rows(
    lapply(detTables, function(table) {
      dbReadTable(con, table)
    })
  )
  if(nrow(allDetections)==0) {
    stop('No detections found for grouping method "', grouping,
         '" in database ', basename(dbFile))
  }
  allEvents <- bind_rows(
    lapply(eventTables, function(table) {
      dbReadTable(con, table)
    })
  )
  soundAcquisition <- dbReadTable(con, 'Sound_Acquisition') %>%
    mutate(UTC = as.POSIXct(as.character(UTC), format='%Y-%m-%d %H:%M:%OS', tz='UTC'),
           Status = str_trim(Status),
           SystemType = str_trim(SystemType)) %>%
    filter(Status=='Start') %>%
    arrange(UTC) %>%
    select(UTC, sampleRate, SystemType) %>%
    distinct() %>%
    data.table() %>% setkey(UTC)
  dbDisconnect(con)

  allDetections <- allDetections %>%
    mutate(BinaryFile = str_trim(BinaryFile),
           UTC = as.POSIXct(as.character(UTC), format='%Y-%m-%d %H:%M:%OS', tz='UTC')) %>%
    select(UTC, Id, UID, parentUID, BinaryFile) %>%
    data.table() %>% setkey(UTC)

  # This rolling join rolls to the first time before. Since we filtered to only starts, it goes back
  # to whatever the last Start was.
  allDetections <- soundAcquisition[allDetections, roll = TRUE] %>%
    data.frame()
  srNa <- which(is.na(allDetections$sampleRate))
  if(length(srNa) == nrow(allDetections)) {
    srReplace <- as.integer(
      readline(prompt = 'No Sample Rate found in SoundAcquisition table. Enter Sample Rate for this data:\n')
    )
    allDetections$sampleRate[srNa] <- srReplace
  } else if(length(srNa) > 0) {
    # get mode
    mode <- which.max(tabulate(allDetections$sampleRate[-srNa]))
    srChoice <- menu(title=paste0('Could not get Sample Rate for all detections from the "SoundAcquistion" table.',
                                  ' Should missing values be replaced with ', mode, '(value found in table).'),
                     choices = c('Yes', 'No (I want to enter my own SR)'))
    srReplace <- switch(srChoice,
                        '1' = mode,
                        '2' = readline(prompt='What Sample Rate should be used?\n'),
                        stop('Sample Rate required for calculations.')
    )
    allDetections$sampleRate[srNa] <- srReplace
  }
  allDetections
}

getBinaryData <- function(dbData, binaryList) {
  dbData <- arrange(dbData, UID)
  # This breaks if 'dbData' doesnt have binaryfile...
  binFile <- dbData$BinaryFile[1]
  allBinFiles <- grep(binFile, binaryList, value=TRUE)
  if(length(allBinFiles)==0) {
    return(NULL)
  } else if(length(allBinFiles)==1) {
    thisBin <- loadPamguardBinaryFile(allBinFiles, keepUIDs=dbData$UID)
    matchSr <- select(dbData, UID, sampleRate) %>%
      distinct() %>% arrange(UID)
    for(i in seq_along(matchSr$UID)) {
      thisBin$data[[i]]$sampleRate <- matchSr$sampleRate[i]
    }
    return(thisBin)
  } else {
    for(bin in allBinFiles) {
      thisBin <- loadPamguardBinaryFile(bin)
      # We've found the right file if UID is in file
      if(dbData$UID[1] %in% names(thisBin$data)) {
        thisBin$data <- thisBin$data[dbData$ClickNo+1]
        for(i in seq_along(dbData$UID)) {
          thisBin$data[[i]]$sampleRate <- dbData$sampleRate[i]
        }
        return(thisBin)
      } else {
        next
      }
    }
    # If we made it here we didnt find a matching file
    return(NULL)
  }
}
