#' @title Get Binary Data for Detections from an Acoustic Event
#'
#' @description Fetches matching binary data from a single or multiple
#'   detections in an \linkS4class{AcousticEvent} object
#'
#' @param acEv a \linkS4class{AcousticEvent} object
#' @param UID the UID(s) of the individual detections to fetch the binary
#'   data for
#'
#' @return a list of \code{PamBinary} objects for each \code{UID}
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom dplyr bind_rows
#' @importFrom PamBinaries loadPamguardBinaryFile
#' @export
#'
getBinaryData <- function(acEv, UID) {
    bins <- bind_rows(
        lapply(acEv@detectors, function(df) {
            df[df[['UID']] %in% UID, c('UID', 'BinaryFile')]
        }))
    if(is.null(bins)) {
        warning('No matches found for UID(s) ', paste(UID, collapse=','), '.')
        return(bins)
    }
    bins <- unique(bins)
    nIn <- sapply(UID, function(x) {
        sum(x == bins[['UID']])
    })
    if(any(nIn == 0)) {
        warning('No matches found for UID(s) ',
                paste(UID[nIn == 0], collapse=','), '.')
    }
    if(any(nIn > 1)) {
        warning('Multiple matches found for UID(s) ',
                paste(UID[nIn > 1], collapse=','), '.')
        print(bins[bins[['UID']] %in% UID[nIn > 1],])
    }
    result <- lapply(unique(bins$BinaryFile), function(bin) {
        # this has full path name
        fullBin <- grep(bin, acEv@files$binaries, value = TRUE)
        if(length(fullBin)==0) {
            warning('Binary file ', bin, ' not found in files slot.')
            return(NULL)
        }
        if(length(fullBin) > 1) {
            warning('More than one binary file found for ', bin, ' only using first one.')
            fullBin <- fullBin[1]
        }
        if(!file.exists(fullBin)) {
            warning('Binary file ', fullBin, ' does not exist, please check (this file name',
                    ' should be the full file path).')
            return(NULL)
        }
        loadPamguardBinaryFile(fullBin, skipLarge = FALSE, convertDate = TRUE,
                               keepUIDs = bins[['UID']][bins$BinaryFile == bin])$data
    })
    # list named by UID as result
    unlist(result, recursive = FALSE)
}
