#' @title Remove a Calibration Function from a PAMrSettings Object
#'
#' @description Remove a calibration function from the "calibration"
#'   slot of a PAMrSettings object
#'
#' @param prs a \linkS4class{PAMrSettings} object to remove a calibration from
#' @param module the module of the calibration function to remove, currently
#'   not needed
#' @return the same \linkS4class{PAMrSettings} object as prs, with the calibration
#'   function removed from the "calibration" slot
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom utils menu
#' @export
#'
removeCalibration <- function(prs, module='ClickDetector') {
    # if(is.null(module)) {
    #     modList <- names(prs@calibration)
    #     modix <- menu(choices = modList, title = 'Choose a module:')
    #     module <- modList[modix]
    # }
    calList <- names(prs@calibration[[module]])
    if(length(calList) == 0) {
        cat('No calibration functions to remove.')
        return(prs)
    }
    dropix <- menu(title = 'Which calibration function should we remove?',
                   choices = calList)
    if(dropix == 0) return(prs)
    dropName <- calList[dropix]
    prs@calibration[[module]] <- prs@calibration[[module]][-dropix]
    # Removing calibration from functions, set back to NULL
    argList <- lapply(prs@functions[[module]], formals)
    hasCal <- which(sapply(argList, function(x) {
        'calibration' %in% names(x) &&
            x[['calibration']] == dropName
    }))
    for(i in hasCal) {
        thisArgs <- argList[[i]]
        # this looks odd, but only way to set NULL without removing from list
        whichCal <- which(names(thisArgs) == 'calibration')
        thisArgs[whichCal] <- list(NULL)
        formals(prs@functions[[module]][[i]]) <- thisArgs
        cat('removed calibration from function', names(argList[i]), '\n')
    }
    prs
}
