#' @title Calculate Inter-Click Interval
#'
#' @description Calculate inter-click interval for click data
#'
#' @param x a list of \linkS4class{AcousticEvent}
#'   objects, or a single \linkS4class{AcousticEvent} object
#' @param time the time measurement to use. \code{start} will use the \code{UTC} value,
#'   \code{peak} will use the \code{peakTime} value if present (currently present in
#'   \code{standardClickCalcs}, this is the time of the peak of the waveform)
#' @param banter result from export_banter
#' @param \dots not used
#'
#' @details Calculates the ICI for each individual detector and across all detectors.
#'   ICI calculation is done by ordering all individual detections by time, then taking
#'   the difference between consecutive detections and taking the mode value.
#'
#' @return ICI data
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @name banterICI
#' @importFrom dplyr bind_rows
#' @export
#'
banterICI <- function(x, time=c('UTC', 'peakTime'), banter=NULL, ...) {
    if(is.null(banter)) {
        banter <- export_banter(x)
    }
    if(inherits(x, 'AcousticEvent')) {
        name <- x@id
        x <- list(x)
        names(x) <- name
    }
    if(is.list(x)) {
        iciList <- lapply(x, function(e) {
            eventICI(e, time, ...)$ici$All
        })
    }
    oldEvents <- banter$events
    evs <- oldEvents$event.id

    oldEvents$ici <- NA
    for(e in evs) {
        oldEvents$ici[oldEvents$event.id == e] <- iciList[[e]]
    }
    if(any(is.na(oldEvents$ici))) {
        warning('No ici values found for events ',
             paste0(oldEvents$event.id[is.na(oldEvents$ici)], collapse=', ')
        )
    }
    banter$events <- oldEvents
    banter
}

#' @rdname banterICI
#' @export
#'
eventICI <- function(x, time=c('UTC', 'peakTime'), ...) {
    if(is.list(x)) {
        return(lapply(x, function(e) {
            eventICI(e, time, ...)
        }))
    }
    detNames <- names(detectors(x))
    detNames <- grep('Click_Detector', detNames, value=TRUE)
    iciList <- vector('list', length = length(detNames) + 1)
    names(iciList) <- c(detNames, 'All')
    for(d in detNames) {
        iciList[[d]] <- data.frame(name=d,
                                   ici=dfICI(detectors(x)[[d]], match.arg(time)),
                                   stringsAsFactors = FALSE)
    }
    iciList[['All']] <- data.frame(name='All',
                                   ici=dfICI(bind_rows(detectors(x)[detNames]), match.arg(time)),
                                   stringsAsFactors = FALSE)
    iciMode <- lapply(iciList, function(i) {
        den <- density(i$ici)
        mode <- den$x[which.max(den$y)]
        if(mode < 0) mode <- 0
        mode
    })
    list(ici = iciMode, iciData = iciList)
}

dfICI <- function(x, time='UTC', plot=FALSE) {
    # check if peakTime is full time or just time within the waveform
    # 1e4 arbitrary, but UTC as numeric will be ~ 1e9
    if(time == 'peakTime' &&
       is.numeric(x$peakTime[1]) &&
       x$peakTime[1] < 1e4) {
        x$peakTime <- as.numeric(x$UTC) + x$peakTime
    }
    if('Channel' %in% colnames(x)) {
        unlist(lapply(unique(x$Channel), function(c) {
            calcICI(x[x$Channel == c, time])
        }))
    } else {
        calcICI(x[, time])
    }
}


calcICI <- function(x) {
    if(length(x) == 1) return(0)
    time <- sort(as.numeric(x))
    ici <- time - c(time[1], time[1:(length(time)-1)])
    ici
}

