#' @title Add Binaries to a PAMrSettings Object
#'
#' @description Adds a new function to the "function" slot in a PAMrSettings
#'   object.
#'
#' @param prs a \linkS4class{PAMrSettings} object to add a database to
#' @param binFolder a folder of binaries to add
#'
#' @return the same \linkS4class{PAMrSettings} object as prs, with the binary
#'   files contained in \code{binFolder} added to the "binaries" slot. Only
#'   binary files for Click Detector and WhistlesMoans modules will be added,
#'   since these are the only types PAMr currently knows how to process
#'   (last updated v 0.7.0)
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom rstudioapi isAvailable selectDirectory
#' @export
#'
addBinaries <- function(prs, binFolder=NULL) {
    binList <- NULL
    if(is.PAMrSettings(binFolder)) {
        binList <- binFolder@binaries$list
        binFolder <- binFolder@binaries$folder
        exists <- file.exists(binList)
        if(any(!exists)) {
            binList <- binList[exists]
            cat(sum(!exists), 'binary files did not exist.\n')
        }
    }
    if(is.null(binFolder)) {
        cat('Please select the folder where the binaries are stored.\n')
        if(!rstudioapi::isAvailable('1.1.287')) {
            binFolder <- choose.dir(caption = 'Choose Binary Folder:')
        } else {
            binFolder <- rstudioapi::selectDirectory(caption = 'Choose Binary Folder:', path = getwd())
        }
    }
    # Case when cancelled, dont error
    if(is.null(binFolder) || is.na(binFolder)) {
        cat('No folder chosen')
        return(prs)
    }
    if(!dir.exists(binFolder)) {
        cat(paste0('Binary folder ', binFolder, ' does not exist'))
        return(prs)
    }
    prs@binaries$folder <- unique(c(prs@binaries$folder, binFolder))
    if(is.null(binList) ||
       length(binList) == 0) {
        cat('Getting list of all binary files in folder. This may take a while...\n')
        # only have functions for Clicks & Whistles right now, filter out so we dont get garbage
        # warning overflow later
        binList <- list.files(binFolder, recursive = TRUE, full.names = TRUE, pattern ='(Clicks|WhistlesMoans).*pgdf$')
    }
    cat('Adding', length(binList), 'binary files from', length(binFolder), 'folders\n')
    prs@binaries$list <- unique(c(prs@binaries$list, binList))
    prs
}
