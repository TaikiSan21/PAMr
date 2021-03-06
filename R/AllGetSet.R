# All get/set
# Get/Set for AcousticEvent class -----------------------------------------

#' @title \code{AcousticEvent} and \code{AcousticStudy} accessors
#'
#' @description Accessors for slots in \linkS4class{AcousticEvent}
#'   and \linkS4class{AcousticStudy} objects
#'
#' @param x a \linkS4class{AcousticEvent} or \linkS4class{AcousticStudy} object
#' @param value value to assign with accessor
#' @param i index of the object to access
#' @param name name of the object to access
#' @param \dots other arguments to pass to methods
#'
#' @return
#' \describe{
#'   \item{id}{a unique id or name for this object}
#'   \item{settings}{a named list of settings for each detector and localization or recorder}
#'   \item{detectors}{a list of detector data frames}
#'   \item{localizations}{list of localizations}
#'   \item{species}{list of species classifications}
#'   \item{files}{list of files used to create this object}
#'   \item{events}{a list of \linkS4class{AcousticEvent} objects}
#'   \item{gps}{a dataframe containing gps data}
#'   \item{prs}{the \linkS4class{PAMrSettings} object used to create this}
#'   \item{effort}{something about effort?}
#'   \item{ancillary}{miscellaneous extra data}
#'   }
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @name PAMr.accessors
#'
#' @importFrom methods setGeneric setMethod validObject
#'
NULL

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('settings', function(x, ...) standardGeneric('settings'))

#' @export
#' @rdname PAMr.accessors
#' @aliases settings
#'
setMethod('settings', 'AcousticEvent', function(x, ...) x@settings)

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('settings<-', function(x, value) standardGeneric('settings<-'))

#' @export
#' @rdname PAMr.accessors
#' @aliases settings
#'
setMethod('settings<-', 'AcousticEvent', function(x, value) {
    x@settings <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('localizations', function(x, ...) standardGeneric('localizations'))

#' @export
#' @rdname PAMr.accessors
#' @aliases localizations
#'
setMethod('localizations', 'AcousticEvent', function(x, ...) x@localizations)

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('localizations<-', function(x, value) standardGeneric('localizations<-'))

#' @export
#' @rdname PAMr.accessors
#' @aliases localizations
#'
setMethod('localizations<-', 'AcousticEvent', function(x, value) {
    x@localizations <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('id', function(x, ...) standardGeneric('id'))

#' @export
#' @rdname PAMr.accessors
#' @aliases id
#'
setMethod('id', 'AcousticEvent', function(x, ...) x@id)

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('id<-', function(x, value) standardGeneric('id<-'))

#' @export
#' @rdname PAMr.accessors
#' @aliases id
#'
setMethod('id<-', 'AcousticEvent', function(x, value) {
    x@id <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('detectors', function(x, ...) standardGeneric('detectors'))

#' @export
#' @rdname PAMr.accessors
#' @aliases detectors
#'
setMethod('detectors', 'AcousticEvent', function(x, ...) x@detectors)

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('detectors<-', function(x, value) standardGeneric('detectors<-'))

#' @export
#' @rdname PAMr.accessors
#' @aliases detectors
#'
setMethod('detectors<-', 'AcousticEvent', function(x, value) {
    x@detectors <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('species', function(x, ...) standardGeneric('species'))

#' @export
#' @rdname PAMr.accessors
#' @aliases species
#'
setMethod('species', 'AcousticEvent', function(x, ...) x@species)

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('species<-', function(x, value) standardGeneric('species<-'))

#' @export
#' @rdname PAMr.accessors
#' @aliases species
#'
setMethod('species<-', 'AcousticEvent', function(x, value) {
    x@species <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('files', function(x, ...) standardGeneric('files'))

#' @export
#' @rdname PAMr.accessors
#' @aliases files
#'
setMethod('files', 'AcousticEvent', function(x, ...) x@files)

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('files<-', function(x, value) standardGeneric('files<-'))

#' @export
#' @rdname PAMr.accessors
#' @aliases files
#'
setMethod('files<-', 'AcousticEvent', function(x, value) {
    x@files <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('ancillary', function(x, ...) standardGeneric('ancillary'))

#' @export
#' @rdname PAMr.accessors
#' @aliases ancillary
#'
setMethod('ancillary', 'AcousticEvent', function(x, ...) x@ancillary)

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('ancillary<-', function(x, value) standardGeneric('ancillary<-'))

#' @export
#' @rdname PAMr.accessors
#' @aliases ancillary
#'
setMethod('ancillary<-', 'AcousticEvent', function(x, value) {
    x@ancillary <- safeListAdd(x@ancillary, value)
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setMethod('[', 'AcousticEvent', function(x, i) {
    x@detectors[i]
})

#' @export
#' @rdname PAMr.accessors
#'
setMethod('[<-', 'AcousticEvent', function(x, i, value) {
    x@detectors[i] <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setMethod('$', 'AcousticEvent', function(x, name) {
    '[['(x@detectors, name)
})

#' @export
#' @rdname PAMr.accessors
#'
setMethod('$<-', 'AcousticEvent', function(x, name, value) {
    x@detectors[[name]] <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setMethod('[[', 'AcousticEvent', function(x, i) {
    '[['(x@detectors, i)
})

#' @export
#' @rdname PAMr.accessors
#'
setMethod('[[<-', 'AcousticEvent', function(x, i, value) {
    x@detectors[[i]] <- value
    validObject(x)
    x
})

#' @importFrom utils .DollarNames
#' @export
#'
.DollarNames.AcousticEvent <- function(x, pattern='') {
    grep(pattern, names(detectors(x)), value=TRUE)
}

#  Get/Set for AcousticStudy class -----------------------------------------------
# #' @title \code{AcousticStudy} accessors
# #'
# #' @description Accessors for slots in \linkS4class{AcousticStudy} objects
#'
# #' @param x a \linkS4class{AcousticStudy} object
# #' @param value value to assign with accessor
# #' @param \dots other arguments to pass to methods
#'
# #' @return
# #' \describe{
# #'   \item{id}{a name or id for this study}
# #'   \item{events}{a list of \linkS4class{AcousticEvent} objects}
# #'   \item{files}{a list of files}
# #'   \item{gps}{a dataframe containing gps data}
# #'   \item{prs}{the \linkS4class{PAMrSettings} object used to create this}
# #'   \item{settings}{a named list of settings for each detector and localization}
# #'   \item{effort}{something about effort?}
# #'   \item{ancillary}{miscellaneous extra data}
# #'   }
#'
# #' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
# #' @name PAMr.accessors
#'
# #' @importFrom methods setGeneric setMethod validObject
#'
# NULL

#' @export
#' @rdname PAMr.accessors
#' @aliases id
#'
setMethod('id', 'AcousticStudy', function(x, ...) x@id)

#' @export
#' @rdname PAMr.accessors
#' @aliases id
#'
setMethod('id<-', 'AcousticStudy', function(x, value) {
    x@id <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#' @aliases files
#'
setMethod('files', 'AcousticStudy', function(x, ...) x@files)

#' @export
#' @rdname PAMr.accessors
#' @aliases files
#'
setMethod('files<-', 'AcousticStudy', function(x, value) {
    x@files <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('gps', function(x, ...) standardGeneric('gps'))

#' @export
#' @rdname PAMr.accessors
#' @aliases gps
#'
setMethod('gps', 'AcousticStudy', function(x, ...) x@gps)

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('gps<-', function(x, value) standardGeneric('gps<-'))

#' @export
#' @rdname PAMr.accessors
#' @aliases gps
#'
setMethod('gps<-', 'AcousticStudy', function(x, value) {
    x@gps <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#' @aliases detectors
#'
setMethod('detectors', 'AcousticStudy', function(x, ...) {
    getDetectorData(x)
})

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('events', function(x, ...) standardGeneric('events'))

#' @export
#' @rdname PAMr.accessors
#' @aliases events
#'
setMethod('events', 'AcousticStudy', function(x, ...) x@events)

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('events<-', function(x, value) standardGeneric('events<-'))

#' @export
#' @rdname PAMr.accessors
#' @aliases events
#'
setMethod('events<-', 'AcousticStudy', function(x, value) {
    x@events <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#' @aliases settings
#'
setMethod('settings', 'AcousticStudy', function(x, ...) x@settings)

#' @export
#' @rdname PAMr.accessors
#' @aliases settings
#'
setMethod('settings<-', 'AcousticStudy', function(x, value) {
    x@settings <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('effort', function(x, ...) standardGeneric('effort'))

#' @export
#' @rdname PAMr.accessors
#' @aliases effort
#'
setMethod('effort', 'AcousticStudy', function(x, ...) x@effort)

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('effort<-', function(x, value) standardGeneric('effort<-'))

#' @export
#' @rdname PAMr.accessors
#' @aliases effort
#'
setMethod('effort<-', 'AcousticStudy', function(x, value) {
    x@effort <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('prs', function(x, ...) standardGeneric('prs'))

#' @export
#' @rdname PAMr.accessors
#' @aliases prs
#'
setMethod('prs', 'AcousticStudy', function(x, ...) x@prs)

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('prs<-', function(x, value) standardGeneric('prs<-'))

#' @export
#' @rdname PAMr.accessors
#' @aliases prs
#'
setMethod('prs<-', 'AcousticStudy', function(x, value) {
    x@prs <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#' @aliases ancillary
#'
setMethod('ancillary', 'AcousticStudy', function(x, ...) x@ancillary)

#' @export
#' @rdname PAMr.accessors
#' @aliases ancillary
#'
setMethod('ancillary<-', 'AcousticStudy', function(x, value) {
    x@ancillary <- safeListAdd(x@ancillary, value)
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('models', function(x, ...) standardGeneric('models'))

#' @export
#' @rdname PAMr.accessors
#' @aliases models
#'
setMethod('models', 'AcousticStudy', function(x, ...) x@models)

#' @export
#' @rdname PAMr.accessors
#'
setGeneric('models<-', function(x, value) standardGeneric('models<-'))

#' @export
#' @rdname PAMr.accessors
#' @aliases models
#'
setMethod('models<-', 'AcousticStudy', function(x, value) {
    x@models <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setMethod('[', 'AcousticStudy', function(x, i) {
    x@events <- x@events[i]
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setMethod('[<-', 'AcousticStudy', function(x, i, value) {
    x@events[i] <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setMethod('$', 'AcousticStudy', function(x, name) {
    '[['(x@events, name)
})

#' @export
#' @rdname PAMr.accessors
#'
setMethod('$<-', 'AcousticStudy', function(x, name, value) {
    x@events[[name]] <- value
    validObject(x)
    x
})

#' @export
#' @rdname PAMr.accessors
#'
setMethod('[[', 'AcousticStudy', function(x, i) {
    '[['(x@events, i)
})

#' @export
#' @rdname PAMr.accessors
#'
setMethod('[[<-', 'AcousticStudy', function(x, i, value) {
    x@events[[i]] <- value
    validObject(x)
    x
})

#' @importFrom utils .DollarNames
#' @export
#'
.DollarNames.AcousticStudy <- function(x, pattern='') {
    grep(pattern, names(events(x)), value=TRUE)
}
