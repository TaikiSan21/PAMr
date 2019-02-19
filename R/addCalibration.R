# The 'show' functions work off of
# AcEv which is currently saving the file name not entire calibration. Could
# change that if we wanted to.
# Also want the clickCalc or whatever function to be able to be called on its
# own, so I probably can't do anything too janky. People probs need to be able
# to test shit with the functions and pull them out or whatever without everything
# going to shit.
#' @title Add a Calibration File to a PAMrSettings Object
#'
#' @description Adds a new calibration function to the "calibration" slot
#'   of a PAMrSettings object.
#'
#' @param prs a \linkS4class{PAMrSettings} object to add a database to
#' @param calFile a calibration file name. Must be csv format with two columns.
#'   The first column must be the frequency, and the second column must be the
#'   sensitivity (in dB), and the columns should be labeled \code{Frequency}
#'   and \code{Sensitivity}.
#' @param module the Pamguard module type this calibration should be applied to,
#'   for now this is only for ClickDetector modules. This is left as an option
#'   for future-proofing purposes but should not be needed.
#' @param calName the name of the calibration function to lookup
#'
#' @return the same \linkS4class{PAMrSettings} object as prs, with the calibration
#'   function added to the \code{calibration} slot.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom utils choose.files read.csv menu
#' @importFrom gam gam s predict.Gam
#' @importFrom seewave spec
#' @export
#'
addCalibration <- function(prs, calFile=NULL, module='ClickDetector') {
    modsAllowed <- c('ClickDetector')
    if(!(module %in% modsAllowed)) {
        warning("I don't know how to add calibration for module type", module)
        return(prs)
    }
    if(is.null(calFile)) {
        cat('Please choose a calibration file (must be csv format).\n')
        calFile <- choose.files(caption = 'Select calibration file:')
    }
    if(length(calFile) == 0) return(prs)

    calFun <- makeCalibration(calFile)
    if(is.null(calFun)) return(prs)

    oldNames <- names(prs@calibration[[module]])
    prs@calibration[module] <- list(c(prs@calibration[[module]], calFun))
    names(prs@calibration[[module]]) <- c(oldNames, calFile)
    prs <- applyCalibration(prs)
    prs
}

# Do checks, return calibration as a function
makeCalibration <- function(calFile) {
    if(!grepl('csv', calFile)) {
        warning('Calibration file must be provided in csv format with columns',
                '"Frequency" and "Sensitivity", calibration has not been added.')
        return(NULL)
    }
    CAL <- read.csv(calFile, header = FALSE, stringsAsFactors = FALSE)
    hasHeader <- grepl('[A-z]', CAL[1, 1]) && grepl('[A-z]', CAL[1, 2])
    if(hasHeader) {
        # if(CAL[1, 1] != 'Frequency' ||
        #    CAL[1, 2] != 'Sensitivity') {
        #     cat('Changing column names of calibration file to "Frequency"',
        #         'and "Sensitivity".')
        # }
        CAL <- CAL[-1, ]
    }
    colnames(CAL) <- c('Frequency', 'Sensitivity')
    if(any(grepl('[^0-9\\.]', CAL[, 1])) ||
       any(grepl('[^0-9\\.]', CAL[, 2]))) {
        warning('Non-numeric values found in calibration file, please check.',
                'Calibration has not been applied.')
        return(NULL)
    }
    CAL$Frequency <- as.numeric(CAL$Frequency)
    CAL$Sensitivity <- as.numeric(CAL$Sensitivity)
    CALMODEL <- gam(Sensitivity ~ s(Frequency, 50), data = CAL)

    # Applies to power spectrum 20* log10(spec).
    # Usually subtracted? Added?
    calFun <- function(wave, sr, ...) {
        specData <- spec(wave = wave, f = sr, norm = FALSE,
                                  correction = 'amplitude', plot = FALSE, ...)
        specData <- data.frame(Frequency = specData[, 1] * 1e3,
                               Sensitivity = specData[, 2])
        specData$Sensitivity <- 20 * log10(specData$Sensitivity)
        specData$Sensitivity[!is.finite(specData$Sensitivity)] <- NA

        predicted <- predict.Gam(CALMODEL, newdata = specData)
        predicted <- predicted - predicted[1]
        # this is + or - WHO KNOWS WTF
        specData$Sensitivity <- specData$Sensitivity - predicted
        specData$Sensitivity <- specData$Sensitivity - max(specData$Sensitivity)
        colnames(specData) <- c('Frequency', 'dB')
        # possibly i should save function and values?
        specData
    }
    class(calFun) <- c('calibration', 'function')
    calFun
}

#' @export
#'
plot.calibration <- function(x, ...) {
    mdl <- get('CALMODEL', envir = environment(x))
    plot(mdl)
}

# Add calibration name to click funs
#' @rdname addCalibration
#' @export
#'
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

        formals(prs@functions[[module]][[i]])['calibration'] <- calName
    }
    prs
}

# do janky environment search to actually get function
#' @rdname addCalibration
#' @export
#'
findCalibration <- function(calName, module = 'ClickDetector') {
    allGlobal <- ls(envir = .GlobalEnv)
    allPrs <- sapply(allGlobal, function(x) {
        obj <- get(x, envir = .GlobalEnv)
        if('PAMrSettings' %in% class(obj)) {
            obj
        } else {
            NULL
        }
    })
    allPrs <- allPrs[!sapply(allPrs, is.null)]
    hasCal <- which(sapply(allPrs, function(x) calName %in% names(x@calibration[[module]])))
    if(length(hasCal) == 0) {
        stop('Could not find calibration function named ', calName,
             ' please re-apply calibration.')
    }
    if(length(hasCal) > 1) {
        warning('Found more than one instance of calibration function named ', calName,
                ', using the first one.')
        useCal <- hasCal[1]
    } else {
        useCal <- hasCal[1]
    }
    allPrs[[useCal]]@calibration[[module]][[calName]]
}
