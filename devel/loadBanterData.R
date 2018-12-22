# BANTER DCL / BW Functions
library(RSQLite)
library(dplyr)
library(stringr)
library(PamBinaries)
library(seewave)
library(banter)
library(rfPermute)
library(gam)
library(data.table)
library(ggplot2)
############################################################
###### YOU MIGHT ADD TO THESE FUNCTIONS
############################################################

# Binary Processing Functions ---------------------------------------------
# These calculate numbers from the binaries to use as predictors.

# These just read basic information from the binary file, no real processing.
binBasic <- function(bin, ...) {
    df <- data.frame(type=bin$type, duration = bin$duration / bin$sampleRate)
    for(a in bin$angles) {
        df <- bind_cols(df, data.frame(angle=a))
    }
    names(df) <- c('type', 'duration', paste0('angle', seq_along(bin$angles)))
    df
}

# Any calculations that operate on the spectrum should go here.
# calFun should be from pascalCalFun, a list of length channel with gam in each spot
binSpec <- function(bin, highpass=10, calFun, new=TRUE, fast=TRUE) {
    result <- list()
    for(chan in 1:ncol(bin$wave)) {
        thisWave <- bin$wave[,chan]
        sr <- bin$sampleRate
        thisWave <- seewave::bwfilter(thisWave, f=sr, n=4, from=highpass*1e3, output='sample')
        ########### TEMP FOR BBW #############
        if(TRUE) {
            # fftSize <- switch(as.character(sr),
            #                   '288000' = 736,
            #                   '256000' = 654,
            #                   '192000' = 456,
            #                   '96000' = 244) # 2.5ms supposed from risso.geographic jasa17
            # 2.5ms window, round to even number
            fftSize <- round(sr * .0025, 0)
            fftSize <- fftSize + (fftSize %% 2)

            # Shortening click wave by getting numpoints=fftsize around peak of wav. Length fft+1
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
        }
        # Calculate the spectrum

        thisSpec <- spec(thisWave, f=sr, wl=512, plot=FALSE, norm=FALSE)

        # Remove frequency below highpass
        # thisSpec <- thisSpec[thisSpec[,1]>highpass,]

        # Do any calculations you want. Here just getting peak frequency.
        peak <- thisSpec[which.max(thisSpec[,2]), 1]*1e3
        thisDf <- data.frame(peak=peak)

        if(new) {
            # TKEO - skip 1st .001s to avoid startup artifacts, energy is 2nd col
            thisTk <- TKEO(thisWave, f=sr, M=1,plot=F)
            # THIS DOESNT WORK REALLY SHORT CLICKS
            tkEnergy <- thisTk[(.001*sr):length(thisWave),2]
            tkDb <- 10*log10(tkEnergy-min(tkEnergy, na.rm=TRUE))
            tkDb <- tkDb - max(tkDb, na.rm=TRUE)
            tkDb[!is.finite(tkDb)] <- NA

            noiseLevel <- median(tkDb, na.rm=TRUE)
            if(is.na(noiseLevel)) {
                noiseLevel <- 0
            }

            noiseThresh <- quantile(thisTk[,2], probs=.4, na.rm=TRUE)*100
            dur <- subset(thisTk, thisTk[,2] >= noiseThresh)
            if(length(dur)==0) {
                dur <- 0
            } else {
                dur <- 1e6*(max(dur[,1])-min(dur[,1]))
            }

            thisSpec <- spec(thisWave, f=sr, wl=fftSize, norm=FALSE, correction='amplitude', plot=FALSE)
            relDb <- 20*log10(thisSpec[,2])
            relDb[!is.finite(relDb)] <- NA

            # Calibration
            newClick <- data.frame(Freq=thisSpec[,1]*1e3, Sens = relDb)
            if(!missing(calFun)) {
                #DO CAL
                predValue <- predict.Gam(calFun[[chan]], newdata=newClick)
                predValue <- predValue - predValue[1]
                clickSens <- relDb-predValue
                clickSens <- clickSens - max(clickSens)
                # browser()
            } else {
                # if no cal, just use original relDb
                clickSens <- relDb - max(relDb)
            }
            calibratedClick <- cbind(newClick$Freq/1e3, clickSens)

            thisDf <- peakTrough(calibratedClick)
            # thisDf <- peakTrough(calibratedClick, plot=runif(1)<5, title=paste(bin$UID, bin$date))

            if(fast) {
                Qfun <- Qfast
            } else Qfun <- seewave::Q
            dbBW10 <- data.frame(Qfun(calibratedClick, f=sr, level=-10, plot=FALSE))
            colnames(dbBW10) <- c('Q_10dB', 'PeakHz_10dB', 'fmin_10dB', 'fmax_10dB', 'BW_10dB')
            dbBW10$centerHz_10dB <- dbBW10$fmax_10dB - (dbBW10$BW_10dB/2)

            dbBW3 <- data.frame(Qfun(calibratedClick, f=sr, level=-3, plot=FALSE))
            colnames(dbBW3) <- c('Q_3dB', 'PeakHz_3dB', 'fmin_3dB', 'fmax_3dB', 'BW_3dB')
            dbBW3$centerHz_3dB <- dbBW3$fmax_3dB - (dbBW3$BW_3dB/2)

            thisDf <- thisDf %>%
                mutate(duration = dur, noiseLevel = noiseLevel) %>%
                cbind(dbBW10, dbBW3)
        }
        # If you wanted to add more, just calculate it and add it as another column in this data frame
        # Ex: thisDf$fakeNumber <- 42

        # names(thisDf) <- paste0('chan', chan, names(thisDf))
        thisDf$Channel <- chan
        result[[chan]] <- thisDf
    }
    result <- bind_rows(result)
    result$UID <- bin$UID
    result
}


############################################################
### YOU PROBABLY DONT WANT TO CHANGE ANYTHING BELOW HERE ###
############################################################

# Read DBs in folder ------------------------------------------------------

detFromDb <- function(dir='./devel/BW' , binDir, binFuns = list(), binList = NULL, skip=0) {
    #### Function input checking
    types <- c('WhistlesMoans', 'ClickDetector', 'Rocca')
    if(class(binFuns) != 'list') {
        stop('binFuns must be a list with names ', paste(types, collapse=', '))
    }
    for(type in types) {
        if(!(type %in% names(binFuns))) {
            cat('No functions provided for ', type, '\n')
            binFuns[[type]] <- list()
        }
    }
    # Warn if some supplied binFuns arent functions
    binFuns <- lapply(binFuns, function(x) {
        if(length(x)==0) {
            return(x)
        } else {
            check <- sapply(x, is.function)
            if(any(!check)) {
                cat('Some items in "binFuns" argument are not functions. \n')
            }
            x[check]
        }
    })
    #### End function input checking

    if(missing(binDir)) {
        binDir <- paste0(dir, '/Binaries')
    }
    dbList <- tools::list_files_with_exts(dir, exts='sqlite3')
    cat('Getting binary list... (this may take a while)\n')
    if(is.null(binList)) {
        binList <- list.files(binDir, pattern='pgdf', recursive=TRUE, full.names=TRUE)
        saveRDS(binList, file='binaryList.RData')
        cat('Saving list of binaries to "binaryList.RData" \n If you re-run this batch without changing any of the',
            'binary files then add the argument binList=readRDS("binaryList.RData") to save some time.')
    }
    cat('Loading databases... \n')
    pb <- txtProgressBar(min=0, max=length(dbList), style=3)
    unlist(lapply(seq_along(dbList), function(dbfile) {
        setTxtProgressBar(pb, dbfile)
        if(dbfile/length(dbList) >= skip) {

            ############ LOAD DB FROM HERE #############
            con <- dbConnect(drv=SQLite(), dbList[dbfile])
            detList <- getDetectorList(con)
            detTables <- purrr::map2(detList$Module_Name, detList$Module_Type, function(module, name) {
                tryCatch({
                    data <- switch(module,
                                   WhistlesMoans = wmReader(con, name, binFuns$WhistlesMoans, binList),
                                   ClickDetector = cdReader(con, name, binFuns$ClickDetector, binList),
                                   Rocca = roccaReader(con, name, binFuns$Rocca, binList)) %>%
                        mutate(db = basename(dbList[dbfile]))
                }, error = function(e) {
                    cat('\nError in db', dbList[dbfile], paste(c('Module: ', 'Name: '), c(module, name)), '\n')
                    print(e)
                    # stop(e)
                    NULL
                })
            })
            names(detTables) <- detList$Module_Type
            dbDisconnect(con)
            whichNotNull <- sapply(detTables, function(x) !is.null(x))
            detTables[whichNotNull]
        }
    }
    ), recursive = FALSE)
}

# Get right tables --------------------------------------------------------

getDetectorList <- function(con) {
    mods <- dbReadTable(con, 'PamguardModules')
    detTables <- mods %>%
        mutate(Module_Name=str_trim(Module_Name),
               Module_Name=gsub(' ', '', Module_Name),
               Module_Type=str_trim(Module_Type),
               Module_Type=gsub(' ', '_', Module_Type)) %>%
        filter(Module_Name %in% c('ClickDetector', 'WhistlesMoans', 'Rocca')) %>%
        distinct(Module_Type, Module_Name)
    detTables
}

# Call type readers -------------------------------------------------------

wmReader <- function(con, name, binFuns, binList) {
    contours <- dbReadTable(con, paste0(name,'_Localised_', name, '_Contours'))
    whistles <- dbReadTable(con, name)
    # If no supplied functions just return this
    if(length(binFuns)==0) {
        whistles <- mutate(whistles, UTC = as.POSIXct(UTC))
        return(whistles)
    } else {
        # DO STUFF
    }
}

cdReader <- function(con, name, binFuns, binList) {
    clicks <- dbReadTable(con, paste0(name, '_Clicks'))
    events <- dbReadTable(con, paste0(name, '_OfflineEvents')) %>%
        mutate(eventType = str_trim(eventType)) %>%
        select(Id, eventType)
    offClicks <- dbReadTable(con, paste0(name, '_OfflineClicks')) %>%
        mutate(BinaryFile = str_trim(BinaryFile),
               UTC = as.POSIXct(as.character(UTC), format='%Y-%m-%d %H:%M:%OS', tz='UTC')) %>%
        select(Id, UTC, EventId, BinaryFile, ClickNo) %>%
        filter(EventId %in% events$Id) %>%
        left_join(events, by = c('EventId' = 'Id')) %>%
        mutate(EventId = paste0(EventId, '_', basename(con@dbname)))
    offClicks <- bind_rows(lapply(split(offClicks, offClicks$EventId), function(ev) {
        ev <- arrange(ev, UTC)
        if(nrow(ev)==1) {
            ev$ICI <- 0
        } else {
            nextTime <- c(ev$UTC[2:nrow(ev)], ev$UTC[nrow(ev)])
            ev$ICI <- as.numeric(nextTime - ev$UTC)
        }
        ev
    })) %>% data.table() %>% setkey(UTC)
    soundAcquisition <- dbReadTable(con, 'Sound_Acquisition') %>%
        mutate(UTC = as.POSIXct(as.character(UTC), format='%Y-%m-%d %H:%M:%OS', tz='UTC'),
               Status=str_trim(Status)) %>%
        filter(Status=='Start') %>%
        arrange(UTC) %>%
        select(UTC, sampleRate) %>%
        data.table() %>% setkey(UTC)

    # This rolling join rolls to the first time before. Since we filtered to only starts, it goes back
    # to whatever the last Start was.
    offClicks <- soundAcquisition[offClicks, roll = TRUE] %>% data.frame()

    # If no supplied functions just return this
    if(length(binFuns)==0) {
        return(offClicks)
    } else {
        bind_rows(lapply(split(offClicks, offClicks$BinaryFile), function(db) {
            db <- arrange(db, ClickNo)
            thisBinFile <- grep(db$BinaryFile[1], binList, value=TRUE)

            # Checking if we have right binary file
            if(length(thisBinFile) == 0) {
                warning(db$BinaryFile[1], ' not found in list')
                NULL
            } else {
                if(length(thisBinFile) > 1) {
                    for(file in thisBinFile) {
                        thisBin <- loadPamguardBinaryFile(file)$data # make faster by changing load to pick specific numbers
                        # If binary doesnt have enough clicks, not a match
                        if(length(thisBin) < max(db$ClickNo) + 1) {
                            next
                        }
                        # If UIDS match or first times match, this is the right file
                        if(('UID' %in% names(db)) &
                           ('UID' %in% names(thisBin[[1]]))) {
                            if(identical(db$UID[1], thisBin[[db$ClickNo[1]+1]]$UID)) {
                                break
                            } else next
                        } else {
                            if(identical(db$UTC[1], thisBin[[db$ClickNo[1]+1]]$date)) {
                                break
                            } else next
                        }
                        cat('Couldnt find match for', db$BinaryFile[1])
                    }
                } else { # case when exactly 1 match
                    thisBin <- loadPamguardBinaryFile(thisBinFile)$data
                }
                if(max(db$ClickNo + 1) > length(thisBin)) {
                    warning('Looks like wrong binary file loaded, ClickNo does not exist in binary. \n',
                            'Problem in ', thisBinFile)
                    db <- filter(db, ClickNo + 1 <= length(thisBin))
                }

                # Adding information to binary needed for functions
                thisBin <- thisBin[db$ClickNo+1]
                for(i in seq_along(thisBin)) {
                    thisBin[[i]]$sampleRate <- db$sampleRate[i]
                }
                ############ TEMP FOR BBW ############
                if(TRUE) {
                    calFun <- pascalCalFun(basename(con@dbname))
                }

                # apply all functions to binaries and append to database data
                cbind(db,
                      bind_cols(lapply(binFuns, function(f) {
                          bind_rows(lapply(thisBin, function(row) {
                              f(row, calFun=calFun) ############ THIS IS MAYBE TEMP SOLUTION JUST FOR NOW #############
                          }))
                      }))
                )
            }
        }))
    }
}

roccaReader <- function(con, name, binFuns, binList) {
    encounter <- dbReadTable(con, paste0(name, '_Encounter_Stats'))
    whistles <- dbReadTable(con, paste0(name, '_Whistle_Stats'))
    # If no supplied functions just return this
    if(length(binFuns)==0) {
        whistles <- mutate(whistles, UTC = as.POSIXct(UTC))
        return(whistles)
    } else {
        # DO STUFF
    }
}


# PASCAL Splitting --------------------------------------------------------

# Take output from detFromDb and split into detectors based on the type col
# JUST SPLIT ONE DATABASE AT A TIME THEN EXPORT TO EVENT
pascalTypeSplit <- function(detectorList, freqBins=NULL) {
    detectorList <- detectorList[which(sapply(detectorList, nrow) != 0)]
    if(is.null(freqBins)) {
        squishList(
            unlist(lapply(detectorList, function(x) split(x, x$type)), recursive=FALSE)
        )
    } else {
        squishList(
            unlist(lapply(detectorList, function(x) {
                x$type <- cut(x[['chan1peak']],  breaks = freqBins, labels=FALSE)
                split(x, x$type)
            }), recursive=FALSE)
        )
    }
}

# Pascal calibration selection

pascalCalFun <- function(stationName) {
    stationName <- gsub('(Station-[0-9\\-]*)_.*$', '\\1', stationName)
    HTI92 <- paste0('Station-', c(1, 3, 5, 6, 8, 9, 10, 12, 15, 16, 19, 22, 23, 24, 25,'26-29', '27-30'))
    if(stationName %in% HTI92) {
        HP92Cal <- read.csv('./devel/BW/Frequency response of HTI-92 hydrophone from SM3M manual.csv')
        gam92 <- gam(Sensitivity~s(Freq, 50), data=HP92Cal)
        HP96Cal <- read.csv('./devel/BW/Frequency response of HTI-96min hydrophone from SM3M manual.csv')
        gam96 <- gam(Sensitivity~s(Freq, 50), data=HP96Cal)
        list(ch0=gam92, ch1=gam96)
    } else {
        HP96Cal <- read.csv('./devel/BW/Frequency response of HTI-96min hydrophone from SM3M manual.csv')
        gam96 <- gam(Sensitivity~s(Freq, 50), data=HP96Cal)
        list(ch0=gam96, ch1=gam96)
    }
}

# Combine things with same name in a list. Does no checking, so dont fuck up
squishList <- function(list) {
    myNames <- unique(names(list))
    result <- vector('list', length=length(myNames))
    names(result) <- myNames
    for(n in myNames) {
        whichThisName <- which(names(list)==n)
        result[[n]] <- bind_rows(list[whichThisName])
    }
    result
}

bbw_data <- function(dir='./devel/BW', binFuns = list(ClickDetector=list(binBasic, binSpec)), binList=NULL, skip=0, ...) {
    detectorData <- detFromDb(dir=dir, binFuns=binFuns, binList=binList, skip=skip) %>%
        pascalTypeSplit(...)
    specCount <- sapply(detectorData, function(x) length(unique(x$eventType)))
    singleSpec <- which(specCount<=1)
    if(length(singleSpec) > 0) {
        warning('Detectors ', names(specCount)[singleSpec], ' have 1 or less species. Removing them.')
        detectorData[-singleSpec]
    } else {
        detectorData
    }
}

bbw_exportDetections <- function(bbwData) {
    for(detector in seq_along(bbwData)) {
        bbwData[[detector]] <- bbwData[[detector]] %>%
            mutate(Id = Id + detector * 1e6) %>%
            rename(event.id=EventId, call.id=Id) %>%
            select(-UTC, -BinaryFile, -ClickNo, -eventType, -type, -db)
    }
    bbwData
}

bbw_exportEvents <- function(bbwData) {
    bind_rows(lapply(bbwData, function(x) {
        distinct(x, eventType, EventId) %>%
            rename(species=eventType, event.id=EventId)
    })) %>% distinct()
}

bbw_makeModel <- function(bbwData) {
    initBanterModel(bbw_exportEvents(bbwData)) %>%
        addBanterDetector(bbw_exportDetections(bbwData), ntree=10, sampsize=1) %>%
        runBanterModel(ntree=100, sampsize=1)
}


# New Q From Seewave ------------------------------------------------------

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


# Helpers -----------------------------------------------------------------

# Running average

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

# Get second peak and trough between. freqBounds is (minDist, maxDist)
# If nothing in this bound, return 0 for 2nd peak and trough

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


