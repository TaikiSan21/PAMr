# This isnt gonna work. Of course you cant store an entire function in the 
# names or args of another function, it just stores as a name that it looks
# up. Will need to do some environment shenanigans or modify how the
# calculateModule part calls all this with calibrations. Needs to be able to
# work when you pass the PRS around somehow. The 'show' functions work off of
# AcEv which is currently saving the file name not entire calibration. Could
# change that if we wanted to.
# Also want the clickCalc or whatever function to be able to be called on its
# own, so I probably can't do anything too janky. People probs need to be able
# to test shit with the functions and pull them out or whatever without everything
# going to shit.
applyCalibration <- function(prs, module = 'ClickDetector') {
    calList <- prs@calibration[[module]]
    if(length(calList) == 0) {
        cat('No calibration functions found in PRS.\n')
        return(prs)
    }
    if(length(calList) == 1) {
        calName <- names(calList)
        calFun <- calList[[1]]
    }
    if(length(calList) > 1) {
        chooseFun <- menu(choices = names(calList),
                          title = 'Which calibration function should be applied?')
        if(chooseFun == 0) {
            warning('You must choose a function.')
            return(prs)
        }
        calName <- names(calList)[chooseFun]
        calFun <- calList[[chooseFun]]
    }
    browser()
    argList <- lapply(prs@functions[[module]], formals)
    hasCal <- which(sapply(argList, function(x) 'calibration' %in% names(x)))
    if(length(hasCal) == 0) {
        cat('Could not find any functions with "calibration" as an input.\n')
        return(prs)
    }
    for(i in hasCal) {
        yn <- menu(choices = c('Yes', 'No'),
                   title = paste0('Should we apply calibration ', calName,
                                  ' to function ', names(argList)[i], '?'))
        if(yn == 2 || yn == 0) next
        argList[[i]]['calibration'] <- calName
        formals(prs@functions[[module]][[i]]) <- argList[[i]]
    }
    prs
}

# check for 'calibration' in function names, then add function as argument
# could do like missing check, if it is set as just spectrum if isnt
# set as calibrated spectrum
addCalibration <- function(prs, calFile, module='ClickDetector') {
    modsAllowed <- c('ClickDetector')
    if(!(module %in% modsAllowed)) {
        warning("I don't know how to add calibration for module type", module)
        return(prs)
    }
    calFun <- getCalibration(calFile)
    if(is.null(calFun)) return(prs)
    
    oldNames <- names(prs@calibration[[module]])
    prs@calibration[module] <- list(c(prs@calibration[[module]], calFun))
    names(prs@calibration[[module]]) <- c(oldNames, calFile)
    prs
}

getCalibration <- function(calFile) {
    if(!grepl('csv', calFile)) {
        warning('Calibration file must be provided in csv format with columns',
                '"Frequency" and "Sensitivity", calibration has not been added.')
        return(NULL)
    }
    cal <- read.csv(calFile, header = FALSE, stringsAsFactors = FALSE)
    hasHeader <- grepl('[A-z]', cal[1, 1]) && grepl('[A-z]', cal[1, 2])
    if(hasHeader) {
        if(cal[1, 1] != 'Frequency' ||
           cal[1, 2] != 'Sensitivity') {
            cat('Changing column names of calibration file to "Frequency" ',
                ' and "Sensitivity".')
        }
        cal <- cal[-1, ]
    }
    colnames(cal) <- c('Frequency', 'Sensitivity')
    if(any(grepl('[^0-9\\.]', cal[, 1])) ||
       any(grepl('[^0-9\\.]', cal[, 2]))) {
        warning('Non-numeric values found in calibration file, please check.',
                'Calibration has not been applied.')
        return(NULL)
    }
    cal$Frequency <- as.numeric(cal$Frequency)
    cal$Sensitivity <- as.numeric(cal$Sensitivity)
    calModel <- gam::gam(Sensitivity ~ gam::s(Frequency, 50), data = cal)
    # function takes in an uncalibrated spectrum and returns calibrated one
    # option to add or subtract? or i need to know when which one is appropriate
    # or should this just replace spec?
    # Applies to power spectrum 20* log10(spec). 
    # Usually subtracted? Added?
    # So 
    calFun <- function(wave, sr, ...) {
        specData <- seewave::spec(wave = wave, f = sr, norm = FALSE,
                                  correction = 'amplitude', plot = FALSE, ...)
        specData <- data.frame(Frequency = specData[, 1] * 1e3,
                               Sensitivity = specData[, 2])
        specData$Sensitivity <- 20 * log10(specData$Sensitivity)
        specData$Sensitivity[!is.finite(specData$Sensitivity)] <- NA
        
        predicted <- predict.Gam(calModel, newdata = specData)
        predicted <- predicted - predicted[1]
        
        # this is + or - WHO KNOWS WTF
        specData$Sensitivity <- specData$Sensitivity - predicted
        specData$Sensitivity <- specData$Sensitivity - max(specData$Sensitivity)
        colnames(specData) <- c('Frequency', 'dB')
        # possibly i should save function and values?
        specData
    }
    calFun
}
# # calibrate spectrogram
# frf <- read.fwf('./devel/frf.tf', c(6, -3, 6))
# colnames(frf) <- c('Freq', 'Sensitivity')
# gamTest <- gam::gam(Sensitivity ~ s(Freq, 50), data=frf)
# newClick <- data.frame(Freq=seq(1, 200e3, 200))
# newClick <- data.frame(Freq = frf$Freq)
# predVals <- gam::predict.Gam(gamTest, newdata = newClick)
# 
# thisSpec <- seewave::spec(thisWave, f=sr, wl=fftSize, norm=FALSE, correction='amplitude', plot=FALSE)
# relDb <- 20*log10(thisSpec[,2])
# relDb[!is.finite(relDb)] <- NA
# 
# # Calibration - I don't have a standardized way of doing this yet
# newClick <- data.frame(Freq=thisSpec[,1]*1e3, Sens = relDb)
# if(!missing(calibration)) {
#     #DO CAL
#     predValue <- gam::predict.Gam(calibration[[chan]], newdata=newClick)
#     predValue <- predValue - predValue[1]
#     clickSens <- relDb-predValue
#     clickSens <- clickSens - max(clickSens)
# } else {
#     # if no cal, just use original relDb
#     clickSens <- relDb - max(relDb)
# }
# calibratedClick <- cbind(newClick$Freq/1e3, clickSens)

        