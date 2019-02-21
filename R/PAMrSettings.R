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
PAMrSettings <- function(db=NULL, binaries=NULL, calibration=NULL) {
    prs <- new('PAMrSettings')
    prs <- addDatabase(prs, db)
    prs <- addBinaries(prs, binaries)
    cat('Default included function(s) are "standardClickCalcs" for the "ClickDetector" module,',
        '"roccaWhistleCalcs" for the "WhistlesMoans" module,',
        'and "standardCepstrumCalcs" for the "Cepstrum" module.\n')
    prs <- addFunction(prs, standardClickCalcs, 'ClickDetector')
    prs <- addFunction(prs, roccaWhistleCalcs, 'WhistlesMoans')
    prs <- addFunction(prs, standardCepstrumCalcs, 'Cepstrum')
    prs <- addCalibration(prs, calibration)
    prs
}
