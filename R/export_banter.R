#' @title Export a List of AcousticEvent Ojbects for Banter Model
#'
#' @description Formats a list of AcousticEvent objects into the structure
#'   needed to run a banter model.
#'
#' @param x a \linkS4class{AcousticStudy} object or
#'   a list of \linkS4class{AcousticEvent} objects
#' @param dropVars a vector of the names of any variables to remove
#' @param dropSpecies a vector of the names of any species to exclude
#' @param training logical flag whether or not this will be used as a
#'   training data set. Training sets must contain a species ID.
#'
#' @return a list with three items, \code{events}, \code{detectors}, and
#'   \code{na}.
#'   \code{events} is a dataframe with two columns. \code{event.id} is a
#'   unique identifier for each event, taken from the names of the event
#'   list. \code{species} is the species classification, taken from the
#'   \code{species} slot labelled \code{id}. \code{detectors} is a list
#'   of data frames containing all the detections and measurements. There is
#'   one list for each unique detector type found in the \code{detectors} slots
#'   of \code{x}. The data frames will only have columns with class
#'   \code{numeric}, \code{integer}, \code{factor}, or \code{logical}, and
#'   will also have columns named \code{UID}, \code{Id}, \code{parentUID},
#'   \code{sampleRate}, \code{Channel}, \code{angle}, and \code{angleError},
#'   removed so that these are not treated as parameters for the banter random
#'   forest model. The dataframes will also have columns \code{event.id} and
#'   \code{call.id} added. \code{na} contains the UIDs and Binary File names
#'   for any detections that had NA values. These cannot be used in the
#'   random forest model and are removed from the exported dataset.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom PAMmisc squishList
#' @importFrom dplyr distinct
#' @importFrom purrr map reduce
#' @export
#'
export_banter <- function(x, dropVars=NULL, dropSpecies=NULL, training=TRUE) {
    if(is.AcousticStudy(x)) {
        x <- events(x)
    }
    sp <- sapply(x, function(e) species(e)$id)
    sp[is.null(sp)] <- NA_character_
    spNa <- sapply(sp, is.na)
    if(training && any(spNa)) {
        warning('Events ', paste(names(x)[which(spNa)], collapse=', '),
             ' do not have a species ID. Data can only be used for prediction, not model training.')
        training <- FALSE
    }

    detNA <- data.frame(UID = character(0), BinaryFile = character(0), stringsAsFactors = FALSE)
    evName <- names(x)
    if(!(length(unique(evName)) == length(evName))) {
        warning('Duplicate event names found, these must be unique for BANTER. Adding numbers to event names.')
        for(i in unique(evName)) {
            evName[evName == i] <- paste0(i, 1:(sum(evName == i)))
        }
        names(x) <- evName
    }
    events <- data.frame(event.id = names(x),
                         stringsAsFactors = FALSE)
    if(training) {
        events$species <- sp
    }
    if(!is.null(dropSpecies)) {
        toDrop <- sp %in% dropSpecies
        sp <- sp[!toDrop]
        events <- events[!toDrop, , drop=FALSE]
        x <- x[!toDrop]
    }
    for(e in seq_along(x)) {
        thisEv <- x[[e]]
        for(d in seq_along(detectors(thisEv))) {
            thisDet <- detectors(thisEv)[[d]]
            if(is.null(thisDet)) next

            thisDet$event.id <- names(x)[e]
            thisDet$call.id <- paste0(names(x)[e], thisDet$UID)
            if('Channel' %in% colnames(thisDet)) {
                thisDet$call.id <- paste0('C', thisDet$Channel, thisDet$call.id)
            }
            colsToDrop <- c('UID', 'Id', 'parentUID', 'sampleRate', 'Channel',
                            'angle', 'angleError', 'peakTime')
            colsToDrop <- unique(c(colsToDrop, dropVars))
            useCols <- lapply(thisDet, class) %in% c('numeric', 'integer', 'factor', 'logical') &
                !(colnames(thisDet) %in% colsToDrop) |
                colnames(thisDet) %in% c('event.id', 'call.id')

            whereNA <- reduce(map(thisDet[, useCols], is.na), `|`)
            detNA <- rbind(detNA, thisDet[whereNA, c('UID', 'BinaryFile')])
            detectors(thisEv)[[d]] <- thisDet[!whereNA, useCols]
        }
        x[[e]] <- thisEv
    }

    dets <- lapply(x, function(e) {
        tmpDet <- detectors(e)
        if(length(tmpDet) == 0) return(NULL)
        tmpDet[sapply(tmpDet, function(y) !is.null(y) && ncol(y) > 2)]
    })
    names(dets) <- NULL
    dets <- squishList(unlist(dets, recursive = FALSE))
    dets <- lapply(dets, distinct)
    if(nrow(detNA) > 0) {
        warning('Removing ', nrow(detNA), ' NA values, to see affected UID(s) and ',
        'BinaryFile(s) check the "na" item in list output.')
    }
    cat('\nCreated data for ', nrow(events), ' events with ',
        sum(sapply(dets, nrow)), ' total detections', sep='')
    if(training) {
        cat(' and ', length(unique(sp)),
            ' unique species: ', paste0(unique(sp), collapse =', '), '.',
            '\nRe-run with dropSpecies argument if any of these are not desired', sep='')
    }
    cat('.', sep='')
    list(events=events, detectors=dets, na=detNA)
}
