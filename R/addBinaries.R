#' @title Add Binaries to a PAMrSettings Object
#'
#' @description Adds a new function to the "function" slot in a PAMrSettings
#'   object.
#'
#' @param prs a \linkS4class{PAMrSettings} object to add a database to
#' @param binfolder a folder of binaries to add
#'
#' @return the same \linkS4class{PAMrSettings} object as prs, with the binary
#'   files contained in \code{binfolder} added to the "binaries" slot
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @export
#'
addBinaries <- function(prs, binfolder) {
    if(missing(binfolder)) {
        cat('Please select the folder where the binaries are stored.\n')
        binfolder <- choose.dir()
    }
    # Case when cancelled, dont error
    if(is.na(binfolder)) return(prs)
    if(!dir.exists(binfolder)) {
        stop(paste0('Binary folder ', binfolder, ' does not exist'))
    }
    prs@binaries$folder <- c(prs@binaries$folder, binfolder)
    cat('Getting list of all binary files in folder. This may take a while...\n')
    binlist <- list.files(binfolder, recursive = TRUE, full.names = TRUE, pattern = 'pgdf')
    prs@binaries$list <- c(prs@binaries$list, binlist)
    prs
}
