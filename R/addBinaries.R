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
#' @export
#'
addBinaries <- function(prs, binFolder=NULL) {
    if(is.null(binFolder)) {
        cat('Please select the folder where the binaries are stored.\n')
        binFolder <- choose.dir(caption = 'Choose Binary Folder:')
    }
    # Case when cancelled, dont error
    if(is.na(binFolder)) return(prs)
    if(!dir.exists(binFolder)) {
        stop(paste0('Binary folder ', binFolder, ' does not exist'))
    }
    prs@binaries$folder <- unique(c(prs@binaries$folder, binFolder))
    cat('Getting list of all binary files in folder. This may take a while...\n')
    # only have functions for Clicks & Whistles right now, filter out so we dont get garbage
    # warning overflow later
    binlist <- list.files(binFolder, recursive = TRUE, full.names = TRUE, pattern ='(Clicks|WhistlesMoans).*pgdf$')
    prs@binaries$list <- unique(c(prs@binaries$list, binlist))
    prs
}
