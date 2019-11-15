#' @title Load and Process Detections from Pamguard
#'
#' @description Loads in acoustic detection data that has been run thorugh
#'   Pamguard. Uses the binary file output from Pamguard and (optionally)
#'   the database used. If a database is used then only detections found
#'   in that database will be processed, and they will be grouped by either
#'   the 'OfflineEvents' table or the 'Detection Group Localiser' modulef
#'   groups. If no database is provided, all detections in all of the
#'   provided binary files will be processed, this can take an extremely
#'   long time. This will also apply a set of functions to the binaray data
#'   loaded. Can also provide a database or csv file to separate detections
#'   from binaries into events by start and end times.
#'
#' @param prs a \linkS4class{PAMrSettings} object containing the databases,
#'   binaries, and functions to use for processing data.
#'   See \code{\link[PAMr]{PAMrSettings}}. Can also be an
#'   \linkS4class{AcousticStudy} object, in which case the \code{prs} slot
#'   will be used.
#' @param mode selector for how to organize your data in to events. \code{db}
#'   will organize by events based on tables in the databases, \code{all} will
#'   put all detections in the binaries into one single event, and \code{time}
#'   will organize into events based on timestamps provided in \code{grouping}.
#' @param id an event name or id for this event, only used for \code{mode='all'}
#' @param sr the sample rate of the data. If reading detections from
#'   a database this will be read in from the SoundAcquisition table, if
#'   \code{mode} is \code{all} or \code{time} this must be specified.
#' @param grouping if \code{mode} is \code{db}, the table to group events by.
#'   Either \code{event} to use the OfflineEvents table, or \code{detGroup} to
#'   use the detection group localiser module groups. If \code{mode} is
#'   \code{time}, this should be a data frame with four columns and 1 row
#'   for each separate event. \code{start} and \code{end} should specify the
#'   start and end time of the event and must be in UTC. \code{id} should
#'   specify a unique id for each event, and \code{species} should provide a
#'   species ID if it is available (this column is optional). This can also
#'   be a character with the file path to a csv file with these columns.
#' @param format the date format for the \code{start} and \code{end} columns
#'   in \code{grouping} if it is a csv. Times are assumed to be UTC. See
#'   ?strptime for details.
#' @param \dots additional arguments to pass onto to different methods
#'
#' @return a list of \linkS4class{AcousticEvent} objects. If reading from a
#'   database, there will be a separate AcousticEvent for each event found
#'   by the grouping method chosen. If just reading all detections in a set
#'   of binary files, all the detections will be in a single AcousticEvent.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom PamBinaries loadPamguardBinaryFile
#' @importFrom PAMmisc squishList
#' @importFrom RSQLite dbConnect dbListTables dbReadTable dbDisconnect SQLite
#' @importFrom stringr str_trim
#' @importFrom data.table data.table setkey
#' @importFrom purrr transpose
#' @import dplyr
#' @export
#'
getPgDetections <- function(prs, mode = c('db', 'all', 'time'), id=NULL,
                            sr=NULL, grouping=c('event', 'detGroup'),
                            format='%Y-%m-%d %H:%M:%OS', ...) {
    if(is.AcousticStudy(prs)) {
        prs <- prs(prs)
    }
    if(!is.PAMrSettings(prs)) {
        stop(paste0(prs, ' is not a PAMrSettings object. Please create one with',
                    ' function "PAMrSettings()"'))
    }
    switch(match.arg(mode),
           'db' = getPgDetectionsDb(prs, grouping, ...),
           'all' = getPgDetectionsAll(prs, id, sr),
           'time' = getPgDetectionsTime(prs, sr, grouping, format, id)
    )
}

#' @importFrom utils setTxtProgressBar txtProgressBar
#'
getPgDetectionsAll <- function(prs, id=NULL, sr=NULL) {
    binList <- prs@binaries$list
    binFuns <- prs@functions
    if(is.null(sr)) {
        sr <- readline(prompt =
                                   paste0('What is the sample rate for this data? ',
                                          '(When processing all binaries, sample rate must be the same) '))
        sr <- as.numeric(sr)
    }
    if(is.null(id)) {
        warning("No Id specified, using today's date.")
        id <- as.character(Sys.Date())
    }
    calibrationUsed <- names(prs@calibration[[1]])
    if(length(calibrationUsed)==0) calibrationUsed <- 'None'

    cat('Processing binary files... \n')
    pb <- txtProgressBar(min=0, max=length(binList), style=3)

    binData <- lapply(binList, function(bin) {
        thisBin <- loadPamguardBinaryFile(bin)
        for(i in seq_along(thisBin$data)) {
            thisBin$data[[i]]$sr <- sr
        }
        thisBinData <- calculateModuleData(thisBin, binFuns)
        setTxtProgressBar(pb, value=which(binList==bin))
        thisBinData
    })
    cat('\n')
    binData <- binData[sapply(binData, function(x) !is.null(x))]
    # for clicks we have split the broad detector into separate ones by classification
    binData <- lapply(binData, function(x) split(x, x$detectorName))
    binData <- unlist(binData, recursive = FALSE)
    binData <- squishList(binData)
    colsToDrop <- c('Id', 'comment', 'sampleRate', 'detectorName', 'parentUID', 'sr')
    binData <- lapply(binData, function(x) {
        dropCols(x, colsToDrop)
    })
    # Should this function store the event ID? Right now its just the name
    # in the list, but is this reliable? Probably not

    acousticEvents <- AcousticEvent(id = id, detectors = binData, settings = list(sr = sr, source = 'Not Found'),
                                    files = list(binaries=binList, db='None', calibration=calibrationUsed))
    acousticEvents
}

#' @importFrom readr read_csv cols col_character
#'
getPgDetectionsTime <- function(prs, sr=NULL, grouping=NULL, format='%Y-%m-%d %H:%M:%OS', id=NULL) {
    # start with checking grouping - parse csv if missing or provided as character and fmt times
    grouping <- checkGrouping(grouping, format)

    binList <- prs@binaries$list
    binFuns <- prs@functions
    allDbs <- prs@db

    # Check for what DB shit should be associated with, get full list of SA data
    # first, gonna match event times to that since its roughly the times assoicated
    # with a database
    saList <- lapply(allDbs, function(db) {
        con <- dbConnect(db, drv=SQLite())
        on.exit(dbDisconnect(con))
        sa <- dbReadTable(con, 'Sound_Acquisition')
        sa$Status <- str_trim(sa$Status)
        sa$SystemType <- str_trim(sa$SystemType)
        sa$UTC <- pgDateToPosix(sa$UTC)
        sa
        })
    names(saList) <- allDbs
    if(is.null(grouping$db)) {
        grouping$db <- NA_character_
    }
    # if they are there and are valid, assume they assigned
    dbToAssign <- which(!file.exists(grouping$db))
    # match db to events
    for(i in dbToAssign) {
        dbPossible <- allDbs[sapply(saList, function(x) {
            inInterval(c(grouping$start[i], grouping$end[i]), x)
        })]

        if(length(dbPossible) == 0 ||
           is.na(dbPossible)) {
            myTitle <- paste0('No matching database found for event ', grouping$id[i],
                              ' based on times, please choose one or select "0" to',
                              ' leave as NA.')
            myChoice <- menu(title = myTitle, choices = allDbs)
            if(myChoice == 0) {
                dbPossible <- NA_character_
            } else {
                dbPossible <- allDbs[myChoice]
            }
        } else if(length(dbPossible) > 1) {
            myTitle <- paste0('Multiple candidate datbases found for event ', grouping$id[i],
                              ' based on times, select one to associate with this event.')
            myChoice <- menu(title = myTitle, choices = dbPossible)
            if(myChoice == 0) {
                dbPossible <- NA_character_
            } else {
                dbPossible <- dbPossible[myChoice]
            }
        }
        grouping$db[i] <- dbPossible
    }
    # on.exit(SAVEMYDATABASE)

    if(is.null(grouping$sr)) {
        grouping$sr <- NA_integer_
    }

    # assign each db in grouping to its unique SRs so we dont have to search again later
    saByDb <- lapply(saList, function(x) unique(x$sampleRate))
    for(d in 1:nrow(grouping)) {
        if(is.na(grouping$db[d])) next
        grouping$sr[d] <- saByDb[grouping$db[d]]
    }

    # were gonna match SR by database, only need manual input if we have any
    # missing DBs
    if(any(is.na(grouping$sr)) &&
       is.null(sr)) {
        sr <- readline(prompt =
                           paste0('Not all events have a database associated with them, ',
                                  'what sample rate should be used for these events?'))
        sr <- as.numeric(sr)
    }
    grouping$sr[is.na(grouping$sr)] <- sr
    # from here can check "simple SR" mode - all SR in DBs and
    # the one we selected are the same, avoid doing shit later

    calibrationUsed <- names(prs@calibration[[1]])
    if(length(calibrationUsed)==0) calibrationUsed <- 'None'

    cat('Processing binary files... \n')
    pb <- txtProgressBar(min=0, max=length(binList), style=3)

    binData <- lapply(binList, function(bin) {
        # should i do here - read in head/foot only, then check those
        # times against grouplist, if none can skip, if one we know
        # what db to match sr with. if more than one... hope they have the
        # same SR? or go fys?
        thisHFOnly <- loadPamguardBinaryFile(bin, skipData=TRUE)$fileInfo
        binBounds <- convertPgDate(c(thisHFOnly$fileHeader$dataDate, thisHFOnly$fileFooter$dataDate))
        evPossible <- (binBounds[1] >= grouping$start & binBounds[1] <= grouping$end) |
            (binBounds[2] >= grouping$start & binBounds[2] <= grouping$end) |
            (binBounds[1] <= grouping$start & binBounds[2] >= grouping$end)
        # if not overlapping any events, skip doing data part mobetta
        if(!any(evPossible)) {
            return(NULL)
        }
        thisBin <- loadPamguardBinaryFile(bin)

        srPossible <- unique(unlist(grouping$sr[evPossible]))
        if(length(srPossible) == 1) {
            for(i in seq_along(thisBin$data)) {
                thisBin$data[[i]]$sr <- srPossible
            }
        } else if(length(srPossible) > 1) {
            evDbs <- unique(grouping$db[evPossible])
            thisSa <- do.call(rbind, saList[evDbs])
            binTimes <- dplyr::bind_rows(lapply(thisBin$data, function(x) {
                list(UID = x$UID, UTC = x$date)
            }))
            binTimes$UTC <- convertPgDate(binTimes$UTC)
            binTimes <- matchSR(binTimes, thisSa)
            for(i in seq_along(thisBin$data)) {
                thisBin$data[[i]]$sr <- binTimes$sampleRate[i]
            }
        }
        thisBinData <- calculateModuleData(thisBin, binFuns)
        setTxtProgressBar(pb, value=which(binList==bin))
        thisBinData
    })

    cat('\n') # space after progress bar finished
    binData <- binData[sapply(binData, function(x) !is.null(x))]
    # for clicks we have split the broad detector into separate ones by classification
    binData <- lapply(binData, function(x) split(x, x$detectorName))
    binData <- unlist(binData, recursive = FALSE)
    binData <- squishList(binData)

    acousticEvents <- vector('list', length = nrow(grouping))
    evName <- as.character(grouping$id)

    colsToDrop <- c('Id', 'comment', 'sampleRate', 'detectorName', 'parentUID', 'sr')
    names(acousticEvents) <- evName

    for(i in seq_along(acousticEvents)) {
        thisData <- lapply(binData, function(x) {
            data <- filter(x, x$UTC >= grouping$start[i], x$UTC <= grouping$end[i])
            if(nrow(data) == 0) return(NULL)
            data
        })
        # Check possible DBs by start/end time of events in sa list earlier
        thisData <- thisData[sapply(thisData, function(x) !is.null(x))]
        binariesUsed <- sapply(thisData, function(x) unique(x$BinaryFile)) %>%
            unlist(recursive = FALSE) %>% unique()
        binariesUsed <- sapply(binariesUsed, function(x) grep(x, binList, value=TRUE), USE.NAMES = FALSE)
        # Check and warning here for empty event
        if(length(thisData) == 0) {
            warning('No detections in Event ', names(acousticEvents)[i])
        }
        thisData <- lapply(thisData, function(x) {
            dropCols(x, colsToDrop)
        })
        thisSr <- grouping$sr[[i]]
        if(is.na(grouping$db[i])) {
            thisSource <- 'Not Found'
        } else {
            filtSa <- saList[[grouping$db[i]]]
            filtSa <- filter(filtSa, filtSa$UTC <= grouping$end[i], filtSa$UTC >= grouping$start[i])
            thisSource <- unique(filtSa$SystemType)
        }

        acousticEvents[[i]] <-
            AcousticEvent(id=evName[i], detectors = thisData, settings = list(sr = thisSr, source = thisSource),
                          files = list(binaries=binariesUsed, db=grouping$db[i], calibration=calibrationUsed))
    }
    if('species' %in% colnames(grouping)) {
        grouping$species <- as.character(grouping$species)
        acousticEvents <- setSpecies(acousticEvents, method = 'manual', value = grouping$species)
    }
    allDbs <- unique(unlist(lapply(acousticEvents, function(x) {
        files(x)$db
    })))
    allBins <- unique(unlist(lapply(acousticEvents, function(x) {
        files(x)$binaries
    })))
    # on.exit() # this cancels the on.exit 'save my grouping' call that is there if you crash
    AcousticStudy(id=id, events = acousticEvents, prs = prs,
                  files = list(db=allDbs, binaries=allBins),
                  ancillary = list(grouping=grouping))
}

#'
getPgDetectionsDb <- function(prs, grouping=c('event', 'detGroup'), id=NULL, ...) {
    allDb <- prs@db
    cat('Processing databases... \n')
    pb <- txtProgressBar(min=0, max=length(allDb), style=3)
    allAcEv <- lapply(allDb, function(db) {
        tryCatch({
            binList <- prs@binaries$list
            binFuns <- prs@functions
            dbData <- getDbData(db, grouping, ...)
            if(is.null(dbData) ||
               nrow(dbData) == 0) {
                warning('No detections found in database ',
                        basename(db), '.')
                setTxtProgressBar(pb, value = which(allDb == db))
                return(NULL)
            }
            thisSr <- unique(dbData$sampleRate)
            if(length(thisSr) > 1) {
                warning('More than 1 sample rate found in database ',
                        basename(db),'.')
            }
            thisSource <- unique(dbData$SystemType)
            dbData <- select(dbData, -SystemType)
            calibrationUsed <- names(prs@calibration[[1]])
            if(length(calibrationUsed)==0) calibrationUsed <- 'None'

            dbData <- lapply(
                split(dbData, dbData$BinaryFile), function(x) {
                    thisBin <- getMatchingBinaryData(x, binList, basename(db))
                    if(length(thisBin)==0) {
                        warning('Could not find the matching binary file for ', x$BinaryFile[1],
                                ' in database ', basename(db))
                        return(NULL)
                    }
                    binData <- calculateModuleData(thisBin, binFuns)
                    if(!is.null(binData)) {
                        binData %>%
                            select(-BinaryFile) %>%
                            inner_join(x, by='UID') %>% distinct()
                    }
                }
            )
            # This is a list for each binary, we want for each detector
            dbData <- dbData[sapply(dbData, function(x) !is.null(x))]

            dbData <- lapply(dbData, function(x) split(x, x$detectorName))
            names(dbData) <- NULL
            dbData <- unlist(dbData, recursive = FALSE)
            dbData <- squishList(dbData)

            # Split into events, then swap from Detector(Events) to Event(Detectors)
            # .names necessary to make sure we have all event numbers
            dbData <- transpose(
                lapply(dbData, function(x) split(x, x$parentUID)),
                .names = unique(unlist(sapply(dbData, function(x) x$parentUID)))
            )

            # Should this function store the event ID? Right now its just the name
            # in the list, but is this reliable? Probably not
            colsToDrop <- c('Id', 'comment', 'sampleRate', 'detectorName', 'parentUID', 'sr')
            acousticEvents <- lapply(dbData, function(ev) {
                ev <- ev[sapply(ev, function(x) !is.null(x))]
                binariesUsed <- sapply(ev, function(x) unique(x$BinaryFile)) %>%
                    unlist(recursive = FALSE) %>% unique()
                binariesUsed <- sapply(binariesUsed, function(x) grep(x, binList, value=TRUE), USE.NAMES = FALSE)
                evId <- paste0(gsub('\\.sqlite3', '', basename(db)), '.', unique(ev[[1]]$parentUID))
                ev <- lapply(ev, function(x) {
                    dropCols(x, colsToDrop)
                })
                AcousticEvent(id = evId, detectors = ev, settings = list(sr = thisSr, source=thisSource),
                              files = list(binaries=binariesUsed, db=db, calibration=calibrationUsed))
            })
            setTxtProgressBar(pb, value = which(allDb == db))
            acousticEvents
        },
        # warning = function(w) {
        #     warning(w)
        #     setTxtProgressBar(pb, value = which(allDb == db))
        #     return(NULL)
        # },
        error = function(e) {
            warning('Error in processing db ', basename(db))
            print(e)
            setTxtProgressBar(pb, value = which(allDb == db))
            return(NULL)
        })
    })
    cat('\n')
    names(allAcEv) <- gsub('\\.sqlite3', '', basename(allDb))
    allAcEv <- unlist(allAcEv, recursive = FALSE)
    allDbs <- unique(unlist(lapply(allAcEv, function(x) {
        files(x)$db
    })))
    allBins <- unique(unlist(lapply(allAcEv, function(x) {
        files(x)$binaries
    })))
    AcousticStudy(id=id, events = allAcEv, prs = prs,
                  files = list(db=allDbs, binaries=allBins))
}

getDbData <- function(db, grouping=c('event', 'detGroup'), label=NULL) {
    # Combine all click/event tables, even by diff detector. Binary will have det name
    con <- dbConnect(SQLite(), db)
    on.exit(dbDisconnect(con))
    tables <- dbListTables(con)
    # Read in event data from either offlineclicks/events or detection
    # group localiser. Click version has common naming convention,
    # det group does not so we have to go look it up. If we are just
    # reading in all the data we only care about SA data
    if(is.null(grouping)) {
        grouping <- c('event', 'detGroup')
    }
    if(length(grouping) > 1) {
        return(
            suppressWarnings(
                bind_rows(
                    lapply(grouping, function(x) {
                        getDbData(db, x)
                    }))
            )
        )
    }
    switch(match.arg(grouping),
           'event' = {
               detTables <- grep('OfflineClicks', tables, value=TRUE)
               eventTables <- grep('OfflineEvents', tables, value=TRUE)
               # eventColumns <- c('UID', 'eventType', 'comment')
               if(is.null(label)) {
                   label <- 'eventType'
               }
               eventColumns <- c('UID', label)
               evName <- 'OE'
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
               # eventColumns <- c('UID', 'Text_Annotation')
               if(is.null(label)) {
                   label <- 'Text_Annotation'
               }
               eventColumns <- c('UID', label)
               evName <- 'DGL'
           },
           {
               stop("I don't know how to group by ", grouping, '.\n')
           }
    )

    if(length(detTables)==0 ||
       length(eventTables)==0) {
        warning('Could not find event tables for grouping method "', grouping,
                '" in database ', basename(db))
        return(NULL)
    }
    allDetections <- bind_rows(
        lapply(detTables, function(table) {
            dbReadTable(con, table)
        })
    )
    if(nrow(allDetections)==0) {
        warning('No detections found for grouping method "', grouping,
                '" in database ', basename(db))
        return(NULL)
    }

    allEvents <- bind_rows(
        lapply(eventTables, function(table) {
            dbReadTable(con, table)
        })
    )
    if(nrow(allEvents)==0) {
        warning('No events found for grouping method "', grouping,
                '" in database ', basename(db))
        return(NULL)
    }

    eventColumns <- eventColumns[eventColumns %in% colnames(allEvents)]
    allEvents <- select_(allEvents, .dots=eventColumns)

    # Do i want all detections in clicks, or only all in events?
    # left_join all det, inner_join ev only
    if(!('UID' %in% names(allEvents)) ||
       !('parentUID' %in% names(allDetections))) {
        cat('UID and parentUID columns not found in database ', basename(db),
            ', these are required to process data. Please upgrade to Pamguard 2.0+.')
        return(NULL)
    }
    allDetections <- inner_join(
        allDetections, allEvents, by=c('parentUID'='UID')
    )

    allDetections <- allDetections %>%
        mutate(BinaryFile = str_trim(BinaryFile),
               # UTC = as.POSIXct(as.character(UTC), format='%Y-%m-%d %H:%M:%OS', tz='UTC')) %>%
               UTC = pgDateToPosix(UTC)) %>%
        select_(.dots=unique(c(eventColumns, 'UTC', 'Id', 'UID', 'parentUID', 'BinaryFile')))

    # rename column to use as label - standardize across event group types
    colnames(allDetections)[which(colnames(allDetections)==label)] <- 'eventLabel'

    allDetections <- matchSR(allDetections, db, extraCols=c('SystemType'))

    # apply str_trim to all character columns
    whichChar <- which(sapply(allDetections, function(x) 'character' %in% class(x)))
    for(i in whichChar) {
        allDetections[, i] <- str_trim(allDetections[, i])
    }
    allDetections <- select(allDetections, -UTC)
    allDetections$UID <- as.character(allDetections$UID)
    allDetections$parentUID <- paste0(evName, allDetections$parentUID)
    allDetections
}

getMatchingBinaryData <- function(dbData, binList, dbName) {
    # dbData here has a single BinaryFile in it, we've split by that before here
    dbData <- arrange(dbData, UID)
    # This breaks if 'dbData' doesnt have binaryfile...
    # Borked if UID mismatch between dems
    binFile <- dbData$BinaryFile[1]
    allBinFiles <- grep(binFile, binList, value=TRUE)
    if(length(allBinFiles)==0) {
        return(NULL)
    }
    if(length(allBinFiles)==1) {
        thisBin <- loadPamguardBinaryFile(allBinFiles, keepUIDs=dbData$UID)
        matchSr <- select(dbData, UID, sampleRate) %>%
            distinct() %>% arrange(UID)
        if(setequal(matchSr$UID, names(thisBin$data))) {
            for(i in seq_along(matchSr$UID)) {
                thisBin$data[[i]]$sr <- matchSr$sampleRate[i]
            }
        } else {
            warning(paste0('UID(s) ', paste0(setdiff(matchSr$UID, names(thisBin$data)), collapse=', '),
                           ' are in database ', dbName, ' but not in binary file ', binFile))
            for(i in names(thisBin$data)) {
                thisBin$data[[i]]$sr <- matchSr$sampleRate[matchSr$UID==i]
            }
        }
        return(thisBin)
    }
    if(length(allBinFiles) > 1) {
        for(bin in allBinFiles) {
            thisBin <- loadPamguardBinaryFile(bin, keepUIDs = dbData$UID)
            # We've found the right file if theres any data
            if(length(thisBin$data) > 0) {
                thisBin$data <- thisBin$data[names(thisBin$data) %in% dbData$UID]
                matchSr <- select(dbData, UID, sampleRate) %>%
                    distinct() %>% arrange(UID)
                if(setequal(matchSr$UID, names(thisBin$data))) {
                    for(i in seq_along(matchSr$UID)) {
                        thisBin$data[[i]]$sr <- matchSr$sampleRate[i]
                    }
                } else {
                    warning(paste0('UID(s) ', paste0(setdiff(matchSr$UID, names(thisBin$data)), collapse=', '),
                                   ' are in database ', dbName, ' but not in binary file ', binFile))
                    for(i in names(thisBin$data)) {
                        thisBin$data[[i]]$sr <- matchSr$sampleRate[matchSr$UID==i]
                    }
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

checkGrouping <- function(grouping, format) {
    if(is.null(grouping)) {
        cat('Please provide a csv file with columns "start", "end", "id", and',
            'optionally "species" to group detections into events.')
        grouping <- file.choose()
    }
    if(inherits(grouping, 'character')) {
        stopifnot(file.exists(grouping))
        grouping <- read_csv(grouping, col_types = cols(.default=col_character()))
    }
    colsNeeded <- c('start', 'end', 'id')
    if(inherits(grouping, 'data.frame')) {
        colnames(grouping) <- tolower(colnames(grouping))
        if(!all(colsNeeded %in% colnames(grouping))) {
            stop('"grouping" must have columns "start", "end" and "id".')
        }
        # if times arent posix, convert and check that it worked
        if(!inherits(grouping$start, 'POSIXct') ||
           !inherits(grouping$end, 'POSIXct')) {
            if(inherits(grouping$start, 'factor')) {
                grouping$start <- as.character(grouping$start)
            }
            if(inherits(grouping$start, 'character')) {
                grouping$start <- as.POSIXct(grouping$start, format=format, tz='UTC')
            }
            if(inherits(grouping$end, 'factor')) {
                grouping$end <- as.character(grouping$end)
            }
            if(inherits(grouping$end, 'character')) {
                grouping$end <- as.POSIXct(grouping$end, format=format, tz='UTC')
            }
            if(any(is.na(grouping$start)) ||
               any(is.na(grouping$end))) {
                warning('Some event start/end times were not able to be converted, please check format.')
            }
            checkDate <- menu(title = paste0('The first event start time is ', grouping$start[1],
                                             ', does this look okay?'),
                              choices = c('Yes, continue processing.',
                                          "No. I'll stop and check grouping data and the time format argument.")
            )
            if(checkDate != 1) {
                stop('Stopped due to invalid event times.')
            }
        }
        grouping$id <- as.character(grouping$id)
    }
    evName <- as.character(grouping$id)
    evTable <- table(evName)
    for(i in unique(evName)) {
        if(evTable[i] == 1) next
        evName[evName == i] <- paste0(i, '_',  1:evTable[i])
    }
    grouping$id <- evName
    grouping
}

# map start/end times in events to db sound acq by overlap
# can just check if "in" a database, if its in multiple ask them
# to choose from the options
# not in range of all, do start/stop within DB so have many start/stop
# can save this grouping thing to ancillary??


mapToDb <- function(events, dbs) {
    dbTimes <- lapply(dbs, function(d) {
        con <- dbConnect(d, drv=SQLite())
        on.exit(dbDisconnect(con))
        sa <- dbReadTable(con, 'Sound_Acquisition')
    })
}
