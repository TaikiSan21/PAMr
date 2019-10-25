#' @title Add GPS Locations to Data
#'
#' @description Add GPS Lat / Long to a variety of types of data.
#'
#' @param x data to add GPS coordinates to. Must have a column \code{UTC}, and
#'   can also have an optional column \code{Channel}
#' @param gps a data frame of GPS coordinates to match to data from \code{x}.
#'   Must have columns \code{UTC}, \code{Latitude}, \code{Longitude}, and
#'   optionally \code{Channel}.
#' @param thresh maximum time in seconds for matching GPS coordinates to data.
#' @param \dots additional arguments for other methods
#'
#' @details Latitude and Longitude coordinates will be matched to the data
#'   by using data.tables rolling join with \code{roll='nearest'}. After the
#'   join is done, the time difference between the matched rows is checked
#'   and any that are greater than the set threshold are set to NA. This is
#'   done to prevent accidentally matching weird things if an incomplete set
#'   of GPS data is provided.
#'
#' @return the same data as \code{x}, with Lat/Long data added
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @name addGps
#' @export
#'
setGeneric('addGps', function(x, gps, thresh = 3600, ...) standardGeneric('addGps'))

#' @rdname addGps
#' @importFrom data.table data.table setkeyv
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @export
#'
setMethod('addGps', 'data.frame', function(x, gps, thresh = 3600, ...) {
    # Check for right columns and proper types
    needCols <- c('UTC', 'Latitude', 'Longitude')
    missingCols <- needCols[!(needCols %in% colnames(gps))]
    if(length(missingCols) > 0) {
        warning('Gps data needs column(s) named', paste(missingCols, collapse = ', '))
        return(x)
    }
    if(!('UTC' %in% colnames(x))) {
        warning('Data needs column UTC.')
        return(x)
    }
    if(!('POSIXct' %in% class(x$UTC))) x$UTC <- pgDateToPosix(x$UTC)
    if(!('POSIXct' %in% class(gps$UTC))) gps$UTC <- pgDateToPosix(gps$UTC)
    # dummies for calculating time difference for threshold check later
    x$dataTime <- x$UTC
    gps$gpsTime <- gps$UTC
    x <- data.table(x)
    gps <- data.table(gps)
    # set keys for the join, using 'Channel' if its present
    if('Channel' %in% colnames(x) &&
       'Channel' %in% colnames(gps)) {
        setkeyv(x, c('Channel', 'UTC'))
        setkeyv(gps, c('Channel', 'UTC'))
        gps <- gps[, c('Channel', 'gpsTime', needCols), with=FALSE]
    } else {
        setkeyv(x, 'UTC')
        setkeyv(gps, 'UTC')
        gps <- gps[, c('gpsTime', needCols), with=FALSE]
    }
    result <- gps[x, roll='nearest'] %>%
        mutate(diff = abs(dataTime - gpsTime),
               Latitude = ifelse(diff > thresh, NA, Latitude),
               Longitude = ifelse(diff > thresh, NA, Longitude),
               UTC = dataTime) %>%
        select(-diff, -gpsTime, -dataTime)
    if(any(is.na(result$Longitude))) {
        warning('Some GPS coordinate matches exceeded time threshold, setting',
                'value to NA.')
    }
    result
})

#' @rdname addGps
#' @export
#'
setMethod('addGps',
          signature(x='AcousticEvent'), function(x, gps, thresh = 3600, ...) {
    # If
    x@detectors <- addGps(x@detectors, gps, thresh, ...)
    x
})

#' @rdname addGps
#' @export
#'
setMethod('addGps', 'list', function(x, gps, thresh = 3600, ...) {
    lapply(x, function(y) addGps(y, gps, thresh, ...))
})

#' @rdname addGps
#' @export
#'
setMethod('addGps', 'AcousticStudy', function(x, gps, thresh = 3600, ...) {
    events(x) <- lapply(events(x), function(y) addGps(y, gps, thresh, ...))
    gps(x) <- gps
    x
})

#' @rdname addGps
#' @export
#'
setMethod('addGps', 'ANY', function(x, gps, thresh = 3600, ...) {
    cat('No addGps method for object type', class(x))
    x
})
