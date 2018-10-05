#' @title Constructor for PAMrSettings Object
#'
#' @description Create a PAMrSettings object. Any values that are not supplied
#'   will be asked for interactively.
#'
#' @param db the full path to a PamGuard database file
#' @param binaries a list with items "folder" containing the directory of the
#'   PamGuard binary files, and "list" containing the full path to each individual
#'   binary file.
#' @param calibration a named list of calibration functions to apply while
#'   applying functions from the "functions" slot. Should named by the
#'   PamGuard module, same as the "functions"
#'
#' @return A PAMrSettings object
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom methods new
#' @importFrom utils choose.files choose.dir
#' @export
#'
PAMrSettings <- function(db, binaries, calibration) {
    prs <- new('PAMrSettings')
    prs <- addDatabase(prs, db)
    prs <- addBinaries(prs, binaries)
    cat('Default included function(s) are "standardClickCalcs" for the "ClickDetector" module.\n')
    prs <- addFunction(prs, standardClickCalcs, 'ClickDetector')

    if(missing(calibration)) {
        cat('Do you have a calibration function for clicks?')
        # lol no you dont
        calibration <- list('ClickDetector'=list())
    }
    prs@calibration <- calibration
    prs
}
