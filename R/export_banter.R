#' @title Export a List of AcousticEvent Ojbects for Banter Model
#'
#' @description Formats a list of AcousticEvent objects into the structure
#'   needed to run a banter model.
#'
#' @param eventList a list of \linkS4class{AcousticEvent} objects.
#'
#' @return a list with two items, \code{events} and \code{detections}.
#'   \code{events} is a dataframe with two columns. \code{event.id} is a
#'   unique identifier for each event, taken from the names of the event
#'   list. \code{species} is the species classification, taken from the
#'   \code{species} slot labelled \code{id}. \code{detections} is a list
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
#' @export
#'
export_banter <- function(eventList) {
    events <- data.frame(event.id = names(eventList),
                         species = sapply(eventList, function(x) {
                             species(x)$id
                         }), stringsAsFactors = FALSE)
    for(e in names(eventList)) {
        thisEv <- eventList[[e]]
        for(d in seq_along(detectors(thisEv))) {
            thisDet <- detectors(thisEv)[[d]]
            thisDet$event.id <- e
            thisDet$call.id <- paste0(e, thisDet$UID)
            if('Channel' %in% colnames(thisDet)) {
                thisDet$call.id <- paste0('C', thisDet$Channel, thisDet$call.id)
            }
            useCols <- lapply(thisDet, class) %in% c('numeric', 'integer', 'factor', 'logical') &
                !(colnames(thisDet) %in% c('UID', 'Id', 'parentUID', 'sampleRate', 'Channel')) |
                colnames(thisDet) %in% c('event.id', 'call.id')
            detectors(thisEv)[[d]] <- thisDet[, useCols]
        }
        eventList[[e]] <- thisEv
    }
    dets <- lapply(eventList, detectors)
    names(dets) <- NULL
    dets <- squishList(unlist(dets, recursive = FALSE))
    list(events=events, detections=dets)
}
