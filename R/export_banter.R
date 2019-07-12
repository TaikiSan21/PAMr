#' @title Export a List of AcousticEvent Ojbects for Banter Model
#'
#' @description Formats a list of AcousticEvent objects into the structure
#'   needed to run a banter model.
#'
#' @param eventList a list of \linkS4class{AcousticEvent} objects.
#' @param reportNA logical, if \code{TRUE} then only the UID's and
#'   Binary File names of any \code{NA} rows will be returned
#'
#' @return a list with two items, \code{events} and \code{detectors}.
#'   \code{events} is a dataframe with two columns. \code{event.id} is a
#'   unique identifier for each event, taken from the names of the event
#'   list. \code{species} is the species classification, taken from the
#'   \code{species} slot labelled \code{id}. \code{detectors} is a list
#'   of data frames containing all the detections and measurements. There is
#'   one list for each unique detector type found in the \code{detectors} slots
#'   of \code{eventList}. The data frames will only have columns with class
#'   \code{numeric}, \code{integer}, \code{factor}, or \code{logical}, and
#'   will also have columns named \code{UID}, \code{Id}, \code{parentUID},
#'   \code{sampleRate}, and \code{Channel} removed so that these are not treated
#'   as parameters for the banter random forest model. The dataframes will also
#'   have columns \code{event.id} and \code{call.id} added.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom PAMmisc squishList
#' @importFrom dplyr distinct
#' @importFrom purrr map reduce
#' @export
#'
export_banter <- function(eventList, reportNA=FALSE) {
    sp <- sapply(eventList, function(x) species(x)$id)
    spNull <- sapply(sp, is.null)
    if(any(spNull)) {
        stop('Events ', paste(names(eventList)[which(spNull)], collapse=', '),
             ' do not have a species ID. Cannot complete export_banter.')
    }

    detNA <- data.frame(UID = character(0), BinaryFile = character(0), stringsAsFactors = FALSE)
    evName <- names(eventList)
    if(!(length(unique(evName)) == length(evName))) {
        warning('Duplicate event names found, these must be unique for BANTER. Adding numbers to event names.')
        for(i in unique(evName)) {
            evName[evName == i] <- paste0(i, 1:(sum(evName == i)))
        }
        names(eventList) <- evName
    }
    events <- data.frame(event.id = names(eventList),
                         species = sp,
                         stringsAsFactors = FALSE)
    for(e in seq_along(eventList)) {
        thisEv <- eventList[[e]]
        for(d in seq_along(detectors(thisEv))) {
            thisDet <- detectors(thisEv)[[d]]
            if(is.null(thisDet)) next

            thisDet$event.id <- names(eventList)[e]
            thisDet$call.id <- paste0(names(eventList)[e], thisDet$UID)
            if('Channel' %in% colnames(thisDet)) {
                thisDet$call.id <- paste0('C', thisDet$Channel, thisDet$call.id)
            }
            useCols <- lapply(thisDet, class) %in% c('numeric', 'integer', 'factor', 'logical') &
                !(colnames(thisDet) %in% c('UID', 'Id', 'parentUID', 'sampleRate', 'Channel')) |
                colnames(thisDet) %in% c('event.id', 'call.id')

            whereNA <- reduce(map(thisDet[, useCols], is.na), `|`)
            detNA <- rbind(detNA, thisDet[whereNA, c('UID', 'BinaryFile')])
            detectors(thisEv)[[d]] <- thisDet[!whereNA, useCols]
        }
        eventList[[e]] <- thisEv
    }
    if(reportNA) {
        return(detNA)
    }
    dets <- lapply(eventList, function(x) {
        tmpDet <- detectors(x)
        tmpDet[sapply(tmpDet, function(y) !is.null(y) && ncol(y) > 2)]
    })
    names(dets) <- NULL
    dets <- squishList(unlist(dets, recursive = FALSE))
    dets <- lapply(dets, distinct)
    if(nrow(detNA) > 0) {
        warning('Removing ', nrow(detNA), ' NA values, re-run export_banter with ',
                'reportNA = TRUE to see affected UID(s) and BinaryFile(s).')
    }
    list(events=events, detectors=dets)
}
