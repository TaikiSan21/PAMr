#' @title Run Custom Calculations on Pamguard Module Data
#'
#' @description Run a list of custom calculations on a Pamguard binary
#'   file.
#'
#' @param binData Pamguard binary data as read in by
#'   \code{\link[PamBinaries]{loadPamguardBinaryFile}}
#' @param binFuns A named list of functions to run on each Pamguard module.
#'   Currently supported modules are 'ClickDetector' and 'WhistlesMoans', a
#'   sample input for binFuns would be list('ClickDetector'=list(clickFun1,
#'   clickFun2), 'WhistlesMoans'=list(wmFun1))
#'
#' @return A data frame with one row for each channel of each detection.
#'   Each row will have the UID, channel number, and name of the detector.
#'   Clicks of different classifications are treated as different detectors
#'   for this purpose, with the classification label number appended to the
#'   detector name. The number of columns will depend on the results of the
#'   calculations from the supplied binFuns.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @import dplyr
#' @export
#'
calculateModuleData <- function(binData, binFuns=list('ClickDetector'=list(standardClickCalcs))) {
    if(length(binData$data) == 0) {
        return(NULL)
    }
    moduleType <- binData$fileInfo$fileHeader$moduleType
    detName <- binData$fileInfo$fileHeader$moduleName
    moduleType <- gsub(' ', '', moduleType)
    # TEMP FIX FOR NOW, MAYBE IS FINE?
    if(moduleType == 'SoundTrapClickDetector') {
        moduleType <- 'ClickDetector'
    }
    if(moduleType == 'ClickDetector' &&
       binData$fileInfo$fileHeader$streamName == 'Trigger Background') {
        moduleType <- 'ClickDetectorTriggerBackground'
        detName <- paste0(detName, '_TriggerBackground')
    }
    # For now, cepstrum-ing like dis
    if(moduleType == 'WhistlesMoans' &&
       grepl('cepstrum', detName, ignore.case = TRUE)) {
        moduleType <- 'Cepstrum'
    }
    if(!(moduleType %in% names(binFuns)) ||
       length(binFuns[[moduleType]])==0) {
        warning("I don't have functions for Module Type ", moduleType)
        # If nothing, just UID and detectorName? Fine for now
        result <- data.frame(UID = as.integer(names(binData$data)))
        result$detectorName <- rep(detName, nrow(result))
        result$BinaryFile <- rep(basename(binData$fileInfo$fileName), nrow(result))
        return(result)
    }
    # Adding this to binFuns for PG version so we always at least get UID
    # ClickDetector gets 1 row for each channel, has 'nChan' in it
    getUID <- function(x) {
        nRep <- if('nChan' %in% names(x)) {
            x$nChan
        } else {
            1
        }
        data.frame(UID =rep(as.character(x$UID), nRep), stringsAsFactors = FALSE)
    }
    result <- switch(
        moduleType,
        'ClickDetector' = {
            allClicks <- doClickCalcs(binData$data, c(getUID, binFuns[['ClickDetector']]))
            # We want each 'type' of click to be separate 'detector'. This is PG only.
            allNames <- bind_rows(
                lapply(binData$data[as.character(allClicks$UID)], function(x) {
                    data.frame(UID=as.character(x$UID),
                               detectorName=unique(c(x$type, unlist(x$annotations))),
                               stringsAsFactors = FALSE)
                })) %>%
                mutate(detectorName = paste(detName, detectorName, sep='_'))
            left_join(allClicks, allNames, by='UID')
        },
        'WhistlesMoans' = {
            allWhistles <- doWhistleCalcs(binData$data, c(getUID, binFuns[['WhistlesMoans']]))
            allWhistles$detectorName <- detName
        },
        'Cepstrum' = {
            allCepstrum <- doCepstrumCalcs(binData$data, c(getUID, binFuns[['Cepstrum']]))
            allCepstrum$detectorName <- detName
        },
        warning("I don't know how to deal with Module Type ", moduleType)
    )
    result$BinaryFile <- basename(binData$fileInfo$fileName)
    result
}

# In general just make sure data has $wave and $sampleRate for clicks?
doClickCalcs <- function(clickData, clickFuns) {
    allClicks <- vector('list', length = length(clickFuns))
    # This just returns a df we will bind to db by UID
    for(f in seq_along(clickFuns)) {
        # Apply this function to each datapoint in binary
        allClicks[[f]] <- bind_rows(
            lapply(clickData, function(oneClick) {
                clickFuns[[f]](oneClick)
            })
        )
    }
    bind_cols(allClicks)
}

# Not sure what this looks like 'in general' like the click one might
# Possibly the PG only version needs to do some heavy modification
# to the slices and shit and then operate on them? Dunno. Basically
# a placeholder right now so doesn't really matter
doWhistleCalcs <- function(whistleData, whistleFuns) {
    # REAL WAY
    allWhistles <- vector('list', length=length(whistleFuns))
    for(f in seq_along(whistleFuns)) {
      allWhistles[[f]] <- bind_rows(
        lapply(whistleData, function(oneWhistle) {
            whistleFuns[[f]](oneWhistle)
        })
      )
    }
    bind_cols(allWhistles)
}

# ceps. THIS IS ALL TERRIBLE BULLSHIT WHY ARE THEY THE SAME FUCKING FIX IT
doCepstrumCalcs <- function(cepstrumData, cepstrumFuns) {
    allCeps <- vector('list', length=length(cepstrumFuns))
    for(f in seq_along(cepstrumFuns)) {
        allCeps[[f]] <- bind_rows(
            lapply(cepstrumData, function(oneCeps) {
                cepstrumFuns[[f]](oneCeps)
            })
        )
    }
    bind_cols(allCeps)
}
