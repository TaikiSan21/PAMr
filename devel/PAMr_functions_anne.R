library(dplyr)
library(RSQLite)
library(stringr)
library(data.table)
library(PamBinaries)
library(seewave)
library(tuneR)
library(gam)

# Calculations - You can make changes to this -----------
# highpass is level for a highpass filter, in kHz.
# winLen is length of window in s, # 2.5ms window size - following Soldevilla paper JASA17
# calFun is used for calibration, but I don't have a standardized way of doing this yet.
# You can add more calculations here - see line 103-104 for simple example
#
standardClickCalcs <- function(bin, calFun, highpass=10, winLen=.0025) {
    result <- list()
    # Do for each channel
    for(chan in 1:ncol(bin$wave)) {
        # We store results in 'thisDf', note channels start at 1 not 0
        thisDf <- data.frame(Channel = chan)
        thisWave <- bin$wave[,chan]
        sr <- bin$sampleRate
        thisWave <- seewave::bwfilter(thisWave, f=sr, n=4, from=highpass*1e3, output='sample')

        # 2.5ms window size - following Soldevilla paper JASA17
        fftSize <- round(sr * winLen, 0)
        fftSize <- fftSize + (fftSize %% 2)

        # Shortening click wave by getting numpoints=fftsize around peak of wav. Length fft+1
        # This removes a lot of the noise samples Pamguard takes - following what Jay/EG did
        wavPeak <- which.max(thisWave)
        tryCatch({
            if(length(thisWave) <= fftSize) {
                # do nothing
            } else if(wavPeak > fftSize/2) {
                if((wavPeak + fftSize/2) <= length(thisWave)) {
                    thisWave <- thisWave[(wavPeak - fftSize/2):(wavPeak + fftSize/2)]
                } else {
                    thisWave <- thisWave[(length(thisWave)-fftSize):length(thisWave)]
                }
            } else {
                thisWave <- thisWave[1:(fftSize+1)]
            }
        }, error = function(e) {
            browser()
        })

        # Do any calculations you want. Here just getting peak frequency.
        # TKEO - skip 1st .001s to avoid startup artifacts, energy is 2nd col
        # This .001s bit doesnt work for really short samples...
        thisTk <- TKEO(thisWave, f=sr, M=1,plot=F)
        if(.001*sr < length(thisWave)) {
            tkEnergy <- thisTk[(.001*sr):length(thisWave),2]
            tkDb <- 10*log10(tkEnergy-min(tkEnergy, na.rm=TRUE))
            tkDb <- tkDb - max(tkDb, na.rm=TRUE)
            tkDb[!is.finite(tkDb)] <- NA

            noiseLevel <- median(tkDb, na.rm=TRUE)
            if(is.na(noiseLevel)) {
                noiseLevel <- 0
            }
        } else {
            noiseLevel <- 0
        }
        thisDf$noiseLevel <- noiseLevel

        # duration defined as time above 40% TKE threshold
        noiseThresh <- quantile(thisTk[,2], probs=.4, na.rm=TRUE)*100
        dur <- subset(thisTk, thisTk[,2] >= noiseThresh)
        if(length(dur)==0) {
            dur <- 0
        } else {
            dur <- 1e6*(max(dur[,1])-min(dur[,1]))
        }
        thisDf$duration <- dur

        thisSpec <- spec(thisWave, f=sr, wl=fftSize, norm=FALSE, correction='amplitude', plot=FALSE)
        relDb <- 20*log10(thisSpec[,2])
        relDb[!is.finite(relDb)] <- NA

        # Calibration - I don't have a standardized way of doing this yet
        newClick <- data.frame(Freq=thisSpec[,1]*1e3, Sens = relDb)
        if(!missing(calFun)) {
            #DO CAL
            predValue <- predict.Gam(calFun[[chan]], newdata=newClick)
            predValue <- predValue - predValue[1]
            clickSens <- relDb-predValue
            clickSens <- clickSens - max(clickSens)
        } else {
            # if no cal, just use original relDb
            clickSens <- relDb - max(relDb)
        }
        calibratedClick <- cbind(newClick$Freq/1e3, clickSens)

        # Simple peak / trough calculations
        thisDf <- cbind(thisDf, peakTrough(calibratedClick))

        # Finding 10/3 dB bandwidth - modified 'Q' function from seewave package
        dbBW10 <- data.frame(Qfast(calibratedClick, f=sr, level=-10, plot=FALSE))
        colnames(dbBW10) <- c('Q_10dB', 'PeakHz_10dB', 'fmin_10dB', 'fmax_10dB', 'BW_10dB')
        dbBW10$centerHz_10dB <- dbBW10$fmax_10dB - (dbBW10$BW_10dB/2)
        thisDf <- cbind(thisDf, dbBW10)

        dbBW3 <- data.frame(Qfast(calibratedClick, f=sr, level=-3, plot=FALSE))
        colnames(dbBW3) <- c('Q_3dB', 'PeakHz_3dB', 'fmin_3dB', 'fmax_3dB', 'BW_3dB')
        dbBW3$centerHz_3dB <- dbBW3$fmax_3dB - (dbBW3$BW_3dB/2)
        thisDf <- cbind(thisDf, dbBW3)

        # If you wanted to add more, just calculate it and add it as another column in this data frame
        # Ex: thisDf$theAnswer <- 42

        result[[chan]] <- thisDf
    }
    # Combine calcs for all channels
    result <- bind_rows(result)
    result$UID <- bin$UID #### DOES THIS NEED TO HAPPEN? ONLY PG SPECIFIC POINT
    result
}

# Banter export -----------
banter_exportDetections <- function(acEv) {
    detList <- lapply(acEv, detectors)
    names(detList) <- NULL
    detList <- squishList(unlist(detList, recursive=FALSE))

    dropNames <- c('UTC', 'sampleRate', 'UID', 'eventType', 'comment', 'Id',
                   'parentUID', 'BinaryFile', 'Channel', 'detectorName')
    for(detector in seq_along(detList)) {
        allNames <- c('event.id', 'call.id', colnames(detList[[detector]]))
        detList[[detector]] <- detList[[detector]] %>%
            rename(event.id=parentUID, call.id=Id) %>%
            select_(.dots=allNames[!(allNames %in% dropNames)])
    }
    detList
}

banter_exportEvents <- function(acEv) {
    bind_rows(lapply(acEv, function(x) {
        bind_rows(
            lapply(detectors(x), function(y) select(y, eventType, parentUID) %>%
                       rename(species=eventType, event.id=parentUID))
        )})) %>% distinct()
}

# Loading data - probably dont change anything below here ---------
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
                                    files = list(binaries=basename(binaryList), database='None', calibration=calibrationUsed))
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
               eventColumns <- c('UID', 'eventType', 'comment')
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
               eventColumns <- c('UID')
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

    eventColumns <- eventColumns[eventColumns %in% colnames(allEvents)]
    allEvents <- select_(allEvents, .dots=eventColumns)

    allDetections <- left_join(
        allDetections, allEvents, by=c('parentUID'='UID')
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
        select_(.dots=unique(c(eventColumns, 'UTC', 'Id', 'UID', 'parentUID', 'BinaryFile'))) %>%
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

calculateModuleData <- function(binData, binFuns) {
    moduleType <- binData$fileInfo$fileHeader$moduleType
    moduleType <- gsub(' ', '', moduleType)
    if(moduleType == 'SoundTrapClickDetector') {
        moduleType <- 'ClickDetector'
    }
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

# Helpers ------------------
squishList <- function(myList) {
    myNames <- unique(names(myList))
    result <- vector('list', length=length(myNames))
    names(result) <- myNames
    for(n in myNames) {
        whichThisName <- which(names(myList)==n)
        thisNameData <- myList[whichThisName]
        thisClasses <- sapply(thisNameData, class)
        # browser()
        result[[n]] <- if(length(whichThisName)==1) {
            thisNameData[[1]]
        } else if('list' %in% thisClasses) {
            thisNameData <- unlist(thisNameData, recursive = FALSE)
            names(thisNameData) <- gsub(paste0(n, '\\.'), '', names(thisNameData))
            squishList(thisNameData)
        } else if(all(thisClasses=='data.frame')) {
            bind_rows(thisNameData)
        } else if(all(thisClasses=='NULL')) {
            next
        } else {
            thisNameData[[1]]
        }
    }
    result
}

runAvg <- function(x, l=5) {
    if(l > length(x)) {
        rep(mean(x, na.rm=TRUE), length(x))
    } else {
        sapply(seq_along(x), function(s) {
            if(s <= (l-1)/2) {
                mean(x[1:(s+(l-1)/2)], na.rm=TRUE)
            } else if((s+(l-1)/2) > length(x)) {
                mean(x[(s-(l-1)/2):length(x)], na.rm=TRUE)
            } else {
                mean(x[(s-(l-1)/2):(s+(l-1)/2)], na.rm=TRUE)
            }
        })
    }
}

peakTrough <- function(spec, freqBounds=c(10, 30), dBMin=-15, plot=FALSE, avg=5, title='hello', export=FALSE) {
    # Default values to return if we dont find other peaks
    peak2 <- 0; peak2dB <- dBMin
    trough <- 0; troughdB <- dBMin
    peak3 <- 0; peak3dB <- dBMin
    trough2 <- 0; trough2dB <- dBMin

    specDf <- data.frame(Freq=spec[,1], dB=runAvg(spec[,2], l=avg)) # Smooth with local average, nearest 5
    wherePeak <- which.max(specDf$dB)
    peak <- specDf$Freq[wherePeak]
    peakdB <- specDf$dB[wherePeak]

    if(length(peak)==0) { # Not sure how this would happen, just in case
        peak <- 0; peakdB <- dBMin
    }

    peak2Df <- specDf %>%
        mutate(before = c(specDf$dB[1], specDf$dB[1:(nrow(specDf)-1)]),
               after = c(specDf$dB[2:nrow(specDf)], specDf$dB[nrow(specDf)]),
               isPeak = (dB > before) & (dB >= after)) %>%
        filter(isPeak,
               ((Freq >= (peak + freqBounds[1])) & (Freq <= (peak + freqBounds[2]))) |
                   ((Freq <= (peak - freqBounds[1])) & (Freq >= (peak - freqBounds[2]))),
               Freq != peak,
               dB >= dBMin)

    if(nrow(peak2Df) > 0) {
        wherePeak2 <- which.max(peak2Df$dB)
        peak2 <- peak2Df$Freq[wherePeak2]
        peak2dB <- peak2Df$dB[wherePeak2]
        peak3Df <- peak2Df %>%
            filter((Freq >= (peak2 + freqBounds[1])) |
                       (Freq <= (peak2 - freqBounds[1])),
                   Freq != peak2)

        if(nrow(peak3Df) > 0) {
            wherePeak3 <- which.max(peak3Df$dB)
            peak3 <- peak3Df$Freq[wherePeak3]
            peak3dB <- peak3Df$dB[wherePeak3]
        }
    }
    allPeaks <- sort(c(peak, peak2, peak3))
    allPeaks <- allPeaks[allPeaks != 0]

    # Find troughs based on num of non-zero peaks. Change nothing if only 1.
    if(length(allPeaks)==2) {
        troughDf <- filter(specDf, Freq > allPeaks[1], Freq < allPeaks[2])
        whereTrough <- which.min(troughDf$dB)
        trough <- troughDf$Freq[whereTrough]
        troughdB <- troughDf$dB[whereTrough]
    } else if(length(allPeaks)==3) {
        troughDf <- filter(specDf, Freq > allPeaks[1], Freq < allPeaks[2])
        whereFirst <- which.min(troughDf$dB)
        first <- troughDf$Freq[whereFirst]
        firstdB <- troughDf$dB[whereFirst]
        troughDf <- filter(specDf, Freq > allPeaks[2], Freq < allPeaks[3])
        whereSecond <- which.min(troughDf$dB)
        second <- troughDf$Freq[whereSecond]
        seconddB <- troughDf$dB[whereSecond]
        # Want lowest trough to be labelled "trough", not "trough2"
        if(firstdB <= seconddB) {
            trough <- first
            troughdB <- firstdB
            trough2 <- second
            trough2dB <- seconddB
        } else {
            trough <- second
            troughdB <- seconddB
            trough2 <- first
            trough2dB <- firstdB
        }
    }

    peakToPeak2 <- abs(peak-peak2)
    peakToPeak3 <- abs(peak-peak3)
    peak2ToPeak3 <- abs(peak2-peak3)

    if(plot) {
        # I DONT THINK THIS WORKS WITH RECTS - CHECK LOGIC LATER
        freqLines <- sort(peak + c(freqBounds, -1*freqBounds))
        graphDf <- data.frame(Freq = c(peak, peak2, peak3, trough, trough2),
                              dB = c(max(specDf$dB), peak2dB, peak3dB, troughdB, trough2dB),
                              Type = c('Highest Peak', 'Second Peak', 'Third Peak', 'Trough / Notch', 'Trough / Notch'))
        g <- ggplot() + geom_line(data=specDf, aes(x=Freq, y=dB)) +
            geom_vline(xintercept=freqLines, color='goldenrod1') +
            geom_hline(yintercept=dBMin, color='blue') +
            geom_point(data=graphDf, aes(x=Freq, y=dB, color=Type), size=3) +
            coord_cartesian(xlim=range(specDf$Freq), ylim=range(specDf$dB)) +
            scale_x_continuous(breaks=seq(0,140,20)) +
            labs(title='Finding Peaks and Troughs', x='Frequency', y='Relative dB') +
            ## FIX ME
            geom_rect(aes(xmin=c(freqLines[1], freqLines[3]), xmax=c(freqLines[2], freqLines[4]), ymin=dBMin, ymax=0, fill='a'), alpha=.1) +
            geom_rect(aes(xmin=c(-5, freqLines[2], freqLines[4]), xmax=c(freqLines[1], freqLines[3], 150), ymin=dBMin, ymax=0, fill='b'), alpha=.15) +
            geom_rect(aes(xmin=c(freqLines[1], freqLines[3]), xmax=c(freqLines[2], freqLines[4]), ymin=-130, ymax=dBMin, fill='c'), alpha=.1) +
            ### END FIX
            scale_fill_manual(values=c('green', 'blue', 'yellow'), labels=c('Range to Search', 'dB Range', 'Frequency Range'), name='') +
            theme(plot.title=element_text(hjust=.5))
        suppressWarnings(print(g))
        if(export) {
            return(g)
        }
    }
    data.frame(peak = peak, peak2 = peak2, peak3 = peak3,
               trough = trough, trough2 = trough2,
               peakToPeak2 = peakToPeak2, peakToPeak3 = peakToPeak3, peak2ToPeak3 = peak2ToPeak3)
}

Qfast <- function(spec = calibratedClick,
                  f = testBin$sampleRate,
                  level = -10,
                  mel = FALSE,
                  plot = FALSE,
                  colval = "red",
                  cexval = 1,
                  fontval = 1,
                  flab = NULL,
                  alab = "Relative amplitude (dB)",
                  type = "l", ...) {

    if (!is.null(f) & mel) {
        f <- 2 * mel(f/2)
    }
    if (is.null(f)) {
        if (is.vector(spec))
            stop("'f' is missing")
        else if (is.matrix(spec))
            f <- spec[nrow(spec), 1] * 2000 * nrow(spec)/(nrow(spec) - 1)
    }
    if (is.matrix(spec)) {
        range <- c(spec[1,1], spec[nrow(spec),1]) ### THERES NO RANGE IF SPEC IS VEC
        # spectest <- spec
        spec <- spec[, 2]
    }
    specMax <- which.max(spec)
    if(length(specMax)==0) {
        return(list(Q = 0, dfreq = 0, fmin = 0, fmax = 0, bdw = 0))
    }
    if (spec[specMax] == 1)
        stop("data must be in dB")
    if (specMax == 1)
        stop("maximal peak cannot be the first value of the spectrum")

    n1 <- length(spec)
    level2 <- spec[specMax] + level
    f0 <- specMax
    f0khz <- (((f0-1)/(n1-1)))*(range[2]-range[1]) + range[1] # These and others below need -1s to properly scale
    specA <- spec[1:f0]
    specB <- spec[f0:length(spec)]
    negA <- which(specA <= level2)
    if(length(negA) == 0) {
        fA <- 1
    } else {
        firstNegA <- max(which(specA <= level2)) # btwn this and next
        fA <- approx(x=spec[firstNegA:(firstNegA+1)], y=firstNegA:(firstNegA+1), xout=level2)$y
    }
    fAkhz <- ((fA-1)/(n1-1)) * (range[2]-range[1]) + range[1]
    nA <- length(specA)
    negB <- which(specB <= level2)
    if(length(negB) == 0) {
        fB <- length(spec)
    } else {
        firstNegB <- min(negB) + (nA - 1) # btwn this and prev
        fB <- approx(x=spec[(firstNegB-1):firstNegB], y=(firstNegB-1):firstNegB, xout=level2)$y
    }
    fBkhz <- ((fB-1)/(n1-1)) * (range[2]-range[1]) + range[1]
    Q <- f0khz/(fBkhz-fAkhz)
    results <- list(Q = Q, dfreq = f0khz, fmin = fAkhz, fmax = fBkhz,
                    bdw = fBkhz - fAkhz)

    results <- lapply(results, function(x) ifelse(length(x)==0, 0, x)) # Temp fix on missing
    if (plot) {
        if (is.null(flab)) {
            if (mel)
                flab <- "Frequency (kmel)"
            else flab <- "Frequency (kHz)"
        }
        x <- seq(range[1], range[2], length.out = n1)
        plot(x = x, y = spec, xlab = flab, ylab = alab, type = type,
             ...)
        arrows(fAkhz, level2, fBkhz, level2, length = 0.1, col = colval,
               code = 3, angle = 15)
        text(paste("Q =", as.character(round(Q, 2))), x = fBkhz,
             y = level2, pos = 4, col = colval, cex = cexval,
             font = fontval)
        invisible(results)
    }
    return(results)
}
# S4 Class Stuff ------------------------------
setClass('DataSettings',
         slots = c(
             sampleRate = 'integer',
             soundSource = 'character',
             otherStuff = 'list'
         ),
         prototype = prototype(sampleRate=192e3L, soundSource='Not Found', otherStuff=list())
)

setValidity('DataSettings',
            function(object) {
                TRUE
            }
)

DataSettings <- function(sampleRate=192e3L, soundSource='Not Found', otherStuff=list()) {
    if(missing(sampleRate)) {
        warning('"sampleRate" not specified, default is 192000Hz')
    }
    if(missing(soundSource)) {
        warning('"soundSource" not found.')
    }
    new('DataSettings', sampleRate=as.integer(sampleRate), soundSource=soundSource, otherStuff=otherStuff)
}

setMethod('show', 'DataSettings',
          function(object) {
              sampleRates <- object@sampleRate
              soundSources <- object@soundSource
              if(length(sampleRates) > 6) {
                  sampleRates <- c(sampleRates[1:6], '...')
              }
              if(length(soundSources) > 6) {
                  soundSources <- c(soundSources[1:6], '...')
              }
              sampleRates <- paste(sampleRates, collapse=', ')
              soundSources <- paste(soundSources, collapse=', ')
              cat('DataSettings object with settings:\nSample Rate(s):', sampleRates,
                  '\nSound Source(s):', soundSources)
          }
)

setClass('VisObsData',
         slots = c(
             detectionTime = 'POSIXct',
             speciesId = 'character',
             groupSizeEst = 'numeric',
             effortStatus = 'character'
         ),
         prototype = prototype(detectionTime = NULL, speciesId = NULL, groupSizeEst = NULL, effortStatus = NULL)
)
# Basic constructor

setValidity('VisObsData',
            function(object) {
                TRUE
            }
)

VisObsData <- function(detectionTime=NULL, speciesId=NULL, groupSizeEst=NULL, effortStatus=NULL) {
    new('visObsData', detectionTime=detectionTime, speciesId=speciesId,
        groupSizeEst=groupSizeEst, effortStatus=effortStatus)
}
setClassUnion('VisOrNULL', c('VisObsData', 'NULL'))

setClass('AcousticEvent',
         slots = c(
             detectors = 'list',
             localizations = 'list',
             settings = 'DataSettings',
             visData = 'VisOrNULL',
             behavior = 'list',
             erdap = 'list',
             specClass = 'list',
             files = 'list'),
         prototype = prototype(detectors=list(), localizations=list(), settings=DataSettings(),
                               visData=NULL, behavior=list(), erdap=list(), specClass=list(),
                               files = list())
)

setValidity('AcousticEvent',
            function(object) {
                valid <- TRUE
                if(length(object@detectors)==0) {
                    cat('AcousticEvent object must have at least one detector. \n')
                    valid <- FALSE
                } else if(is.null(names(object@detectors))) {
                    cat('All detectors in the "detectors" slot must be named. \n')
                    valid <- FALSE
                }
                valid
            }
)
# Basic constructor
AcousticEvent <- function(detectors=list(), localizations=list(), settings=DataSettings(), visData=NULL,
                          behavior=list(), erdap=list(), specClass=list(), files=list()) {
    new('AcousticEvent', detectors=detectors, localizations=localizations, settings=settings,
        visData=visData, behavior=behavior, erdap=erdap, specClass=specClass, files=files)
}

setMethod('show', 'AcousticEvent',
          function(object) {
              cat('AcousticEvent object with', length(object@detectors), 'detector(s): \n')
              cat(paste(names(object@detectors), collapse=', '))
          }
)

# get/set -----------------
setGeneric('settings', function(x, ...) standardGeneric('settings'))
setMethod('settings', 'AcousticEvent', function(x, ...) x@settings)
setGeneric('settings<-', function(x, value) standardGeneric('settings<-'))
setMethod('settings<-', 'AcousticEvent', function(x, value) {
    x@settings <- value
    validObject(x)
    x
})

setGeneric('localizations', function(x, ...) standardGeneric('localizations'))
setMethod('localizations', 'AcousticEvent', function(x, ...) x@localizations)
setGeneric('localizations<-', function(x, value) standardGeneric('localizations<-'))
setMethod('localizations<-', 'AcousticEvent', function(x, value) {
    x@localizations <- value
    validObject(x)
    x
})

setGeneric('detectors', function(x, ...) standardGeneric('detectors'))
setMethod('detectors', 'AcousticEvent', function(x, ...) x@detectors)
setGeneric('detectors<-', function(x, value) standardGeneric('detectors<-'))
setMethod('detectors<-', 'AcousticEvent', function(x, value) {
    x@detectors <- value
    validObject(x)
    x
})

setGeneric('visData', function(x, ...) standardGeneric('visData'))
setMethod('visData', 'AcousticEvent', function(x, ...) x@visData)
setGeneric('visData<-', function(x, value) standardGeneric('visData<-'))
setMethod('visData<-', 'AcousticEvent', function(x, value) {
    x@visData <- value
    validObject(x)
    x
})

setGeneric('behavior', function(x, ...) standardGeneric('behavior'))
setMethod('behavior', 'AcousticEvent', function(x, ...) x@behavior)
setGeneric('behavior<-', function(x, value) standardGeneric('behavior<-'))
setMethod('behavior<-', 'AcousticEvent', function(x, value) {
    x@behavior <- value
    validObject(x)
    x
})

setGeneric('erdap', function(x, ...) standardGeneric('erdap'))
setMethod('erdap', 'AcousticEvent', function(x, ...) x@erdap)
setGeneric('erdap<-', function(x, value) standardGeneric('erdap<-'))
setMethod('erdap<-', 'AcousticEvent', function(x, value) {
    x@erdap <- value
    validObject(x)
    x
})

setGeneric('specClass', function(x, ...) standardGeneric('specClass'))
setMethod('specClass', 'AcousticEvent', function(x, ...) x@specClass)
setGeneric('specClass<-', function(x, value) standardGeneric('specClass<-'))
setMethod('specClass<-', 'AcousticEvent', function(x, value) {
    x@specClass <- value
    validObject(x)
    x
})

setGeneric('files', function(x, ...) standardGeneric('files'))
setMethod('files', 'AcousticEvent', function(x, ...) x@files)
setGeneric('files<-', function(x, value) standardGeneric('files<-'))
setMethod('files<-', 'AcousticEvent', function(x) {
    x@files <- value
    validObject(x)
    x
})

setGeneric('sampleRate', function(x, ...) standardGeneric('sampleRate'))
setMethod('sampleRate', 'DataSettings', function(x, ...) x@sampleRate)
setGeneric('sampleRate<-', function(x, value) standardGeneric('sampleRate<-'))
setMethod('sampleRate<-', 'DataSettings', function(x, value) {
    x@sampleRate <- as.integer(value)
    validObject(x)
    x
})
setGeneric('soundSource', function(x, ...) standardGeneric('soundSource'))
setMethod('soundSource', 'DataSettings', function(x, ...) x@soundSource)
setGeneric('soundSource<-', function(x, value) standardGeneric('soundSource<-'))
setMethod('soundSource<-', 'DataSettings', function(x, value) {
    x@soundSource <- value
    validObject(x)
    x
})
