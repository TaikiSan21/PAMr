# Latest PG version data testing
library(PamBinaries)
library(dplyr)
library(RSQLite)
library(stringr)
library(data.table)

dbFile <- './devel/ArrayTest/Database/testmuthafuckaaa929.sqlite3'
binDir <- './devel/ArrayTest/TestBin'
binList <- list.files(binDir, full.names=TRUE, pattern='pgdf', recursive=T)

# Possible that we don't want 'Manual Click Train Detection' in events (comment)
# Need to count the click classifications as different detectors. 'type' field in
# binary, or really the click annotation whatever from new binary

# Date shit for trying to get millis - safer to just keep the millis column and use when
# we need to?

# We may need a check after the big lapply for size consistency
# If something goes wrong in one calculation somehow we wont be able
# to bind rows.
# OTHER WAYS: If you want everything then you arent really getting
# your detections from DB. Only from binaries. Possible that we
# want to have a different function that says 'do all shit' that
# gets called instead of this at the level before this. Probably
# easier than reconfiguring all of this? Still missing SR if
# you dont have a database to work from...
getDetectionsFromDb <- function(dbFile, binaryList, binFuns, skip=FALSE) {
    # PROBLEM multiple click detectors multiple offline events. Go one at a time
    eventDetections <- getDbData(dbFile) # this only relevant for our way.
    thisSr <- unique(eventDetections$sampleRate)
    if(length(thisSr) > 1) {
        warning('More than 1 sample rate found in database ',
                basename(dbFile),'.')
    }
    thisSource <- unique(eventDetections$SystemType)
    eventDetections <- select(eventDetections, -SystemType)
    calibrationUsed <- 'None'
    # This whole part is fucked if we want to do it differently, eg just
    # want to do every binary file
    eventDetections <- lapply(
        split(eventDetections, eventDetections$BinaryFile), function(dbData) {
            thisBin <- getMatchingBinaryData(dbData, binaryList, skipLarge=skip)
            if(length(thisBin)==0) {
                warning('Could not find the matching binary file for ', dbData$BinaryFile[1],
                        ' in database ', basename(dbFile))
                return(NULL)
            }

            binData <- calculateModuleData(thisBin, binFuns)

            # dbData$UID <- as.integer(dbData$UID)
            # binData$UID <- as.integer(binData$UID)
            # We could make the getSTUFFData functions return a list where one is always 'dataframe' so we can
            # Store other crap like waves. Or since we are storing the binary files and the calibration function
            # we can just have a helper for AcEvs that says 'go get me a fucking waveform'
            # Then maybe files should have full path? And does binary list belong somewhere? Or just
            # write it to an RData. 46Mb in mem, 844kb in disk
            left_join(dbData, binData, by='UID') %>% distinct()
        })
    # This is a list for each binary, we want for each detector
    eventDetections <- eventDetections[sapply(eventDetections, function(x) !is.null(x))]
    binaryUsed <- names(eventDetections)

    names(eventDetections) <- sapply(eventDetections, function(x) x$detectorName[1])
    eventDetections <- squishList(eventDetections)

    # Split into events, then swap from Detector(Events) to Event(Detectors)
    eventDetections <- purrr::transpose(
        lapply(eventDetections, function(x) split(x, x$parentUID))
    )

    # Should this function store the event ID? Right now its just the name
    # in the list, but is this reliable? Probably not
    acousticEvents <- lapply(eventDetections, function(ev) {
        ev <- ev[sapply(ev, function(x) !is.null(x))]
        AcousticEvent(detectors = ev, settings = DataSettings(sampleRate = thisSr, soundSource=thisSource),
                      files = list(binaries=binaryUsed, database=basename(dbFile), calibration=calibrationUsed))
    })
    acousticEvents
}

# Check for binary file we need in list. Hopefully exactly one match, handles if not
# NOTE: if dbData$UID doesnt exist, will return NULL and read all binary data
getMatchingBinaryData <- function(dbData, binaryList, skipLarge=FALSE) {
    dbData <- arrange(dbData, UID)
    # This breaks if 'dbData' doesnt have binaryfile...
    binFile <- dbData$BinaryFile[1]
    allBinFiles <- grep(binFile, binaryList, value=TRUE)
    if(length(allBinFiles)==0) {
        return(NULL)
    } else if(length(allBinFiles)==1) {
        thisBin <- loadPamguardBinaryFile(allBinFiles, keepUIDs=dbData$UID, skipLarge=skipLarge)
        matchSr <- select(dbData, UID, sampleRate) %>%
            distinct() %>% arrange(UID)
        for(i in seq_along(matchSr$UID)) {
            thisBin$data[[i]]$sampleRate <- matchSr$sampleRate[i]
        }
        return(thisBin)
    } else {
        for(bin in allBinFiles) {
            thisBin <- loadPamguardBinaryFile(bin, skipLarge=skipLarge)
            # We've found the right file if the UIDs match.
            if(dbData$UID[1] == thisBin$data[[dbData$ClickNo[1]+1]]$UID) {
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

# Get all the stuff we want out of the database
# Returns UTC, SR, Recording System, Id, UID, EventUID, BinaryFile, ClickNo, Amplitude?, EventId for ours
# Problem if we cant do this from db - wont know the SR until we know the time for what we want
# I mean you can do it, but thats dumb. I guess you could pass out the entire SoundAcquisition
# filtered table... is it reasonable to read all the binar
getDbData <- function(dbFile) {
    # Combine all click/event tables, even by diff detector. Binary will have det name
    con <- dbConnect(SQLite(), dbFile)
    #### FROM HERE IS OUR STYLE ####
    # Possibly rename all this
    allOffDetections <- bind_rows(
        lapply(grep('OfflineClicks', dbListTables(con), value=TRUE), function(table) {
            dbReadTable(con, table)
        })
    )
    allOffEvents <- bind_rows(
        lapply(grep('OfflineEvents', dbListTables(con), value=TRUE), function(table) {
            dbReadTable(con, table)
        })
    )

    allOffDetections <- allOffDetections %>%
        mutate(BinaryFile = str_trim(BinaryFile),
               UTC = as.POSIXct(as.character(UTC), format='%Y-%m-%d %H:%M:%OS', tz='UTC')) %>%
        select(UTC, Id, UID, parentUID, BinaryFile, ClickNo, Amplitude, EventId) %>%
        data.table() %>% setkey(UTC)
    #### TO HERE ####
    soundAcquisition <- dbReadTable(con, 'Sound_Acquisition') %>%
        mutate(UTC = as.POSIXct(as.character(UTC), format='%Y-%m-%d %H:%M:%OS', tz='UTC'),
               Status = str_trim(Status),
               SystemType = str_trim(SystemType)) %>%
        filter(Status=='Start') %>%
        arrange(UTC) %>%
        select(UTC, sampleRate, SystemType) %>%
        data.table() %>% setkey(UTC)
    dbDisconnect(con)
    # This rolling join rolls to the first time before. Since we filtered to only starts, it goes back
    # to whatever the last Start was.
    allOffDetections <- soundAcquisition[allOffDetections, roll = TRUE] %>%
        data.frame()
    srNa <- which(is.na(allOffDetections$sampleRate))
    if(length(srNa) == nrow(allOffDetections)) {
        srReplace <- as.integer(
            readline(prompt = 'No Sample Rate found in SoundAcquisition table. Enter Sample Rate for this data:\n')
        )
    } else if(length(srNa) > 0) {
        # get mode
        mode <- which.max(tabulate(allOffDetections$sampleRate[-srNa]))
        srChoice <- menu(title=paste0('Could not get Sample Rate for all detections from the "SoundAcquistion" table.',
                                      ' Should missing values be replaced with ', mode, '(value found in table).'),
                         choices = c('Yes', 'No (I want to enter my own SR)'))
        srReplace <- switch(srChoice,
                            '1' = mode,
                            '2' = readline(prompt='What Sample Rate should be used?\n'),
                            stop('Sample Rate required for calculations.')
        )} else {
            return(allOffDetections)
        }
    allOffDetections$sampleRate[srNa] <- srReplace
    allOffDetections
}

# Process click calculations on binaries
doClickCalcs <- function(binData, binFuns) {
    allClicks <- vector('list', length = length(binFuns))
    # This just returns a df we will bind to db by UID
    for(f in seq_along(binFuns)) {
        # Apply this function to each datapoint in binary
        allClicks[[f]] <- bind_rows(
            lapply(binData$data, function(oneClick) {
                binFuns[[f]](oneClick)
            })
        )
    }
    allClicks <- bind_cols(allClicks)
    # We want each 'type' of click to be separate 'detector'
    detName <- binData$fileInfo$fileHeader$moduleName
    allNames <- bind_rows(
        lapply(binData$data[as.character(allClicks$UID)], function(x) {
            data.frame(UID=x$UID,
                       detectorName=unique(c(x$type, unlist(x$annotations))))
        })) %>%
        mutate(detectorName = paste(detName, detectorName, sep='_'))
    allClicks <- left_join(allClicks, allNames, by='UID')
    return(allClicks)
}

testClickFun <- function(binData) {
    result <- list()
    for(c in 1:ncol(binData$wave)) {
        x <- binData$sampleRate * binData$angles
        y <- binData$channelMap + sum(binData$wave)
        result[[c]] <- data.frame(WHAT=x, YEAHH=y, chan=c)
    }
    bind_rows(result)
}

doWhistleCalcs <- function(binData, binFuns) {
    # REAL WAY
    # allWhistles <- vector('list', length=length(binFuns))
    # for(f in seq_along(binFuns)) {
    #   allWhistles[[f]] <- bind_rows(
    #     lapply(binData$data, function(oneWhistle) {
    #       binFuns[[f]](oneWhistle)
    #     })
    #   )
    # }
    # allWhistles <- bind_cols(allWhistles)

    # TEMP NO FUNCTIONS
    allWhistles <- bind_rows(
        lapply(binData$data, function(oneWhist) {
            data.frame(UID=oneWhist$UID, shit='whistles')
        })
    )
    detName <- binData$fileInfo$fileHeader$moduleName
    allWhistles$detectorName <- detName
    return(allWhistles)
}

# Screw that, feed them all in here
# can I just cbind dbData, binFuns[moduleType](binData)
# Could have 'Calibration' also in the binFuns list.
# Oh wait not everything needs cal? Only clicks right now. hmm...
# Can just apply calibration within the appropriate moduleType
# I think we will always have to do this once per function,
# it doesnt make sense to make the spectrum once then
# feed that to other things.
calculateModuleData <- function(binData, binFuns) {
    moduleType <- binData$fileInfo$fileHeader$moduleType
    moduleType <- gsub(' ', '', moduleType)

    if(!(moduleType %in% names(binFuns)) ||
       length(binFuns[moduleType])==0) {
        warning("I don't have functions for Module Type ", moduleType)
        # If nothing, just UID and detectorName? Fine for now
        result <- data.frame(UID = as.integer(names(binData$data)),
                             detectorName = binData$fileInfo$fileHeader$moduleName)
        return(result)
    }
    result <- switch(moduleType,
                     'ClickDetector' = doClickCalcs(binData, binFuns[['ClickDetector']]), # maybe just binFuns if we do cal here
                     'WhistlesMoans' = doWhistleCalcs(binData, binFuns[['WhistlesMoans']]),
                     warning("I don't know how to deal with Module Type ", moduleType)
    )
    result
}
