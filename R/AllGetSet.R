# All get/set
# Get/Set for AcousticEvent class -----------------------------------------

#' @title \code{AcousticEvent} accessors
#'
#' @description Accessors for slots in \linkS4class{AcousticEvent} objects
#'
#' @param x a \linkS4class{AcousticEvent} object
#' @param value value to assign with accessor
#' @param \dots other arguments to pass to methods
#'
#' @return
#' \describe{
#'   \item{id}{a unique id or name for this event}
#'   \item{settings}{a \linkS4class{DataSettings} object}
#'   \item{detectors}{a list of detector data frames}
#'   \item{localizations}{list of localizations}
#'   \item{visData}{a \linkS4class{VisObsData} object}
#'   \item{behavior}{behavior data}
#'   \item{erdda}{environmental data}
#'   \item{species}{list of species classifications}
#'   \item{files}{list of files used to create this object}
#'   }
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @name AcousticEvent.accessors
#'
#' @importFrom methods setGeneric setMethod validObject
#'
NULL

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('settings', function(x, ...) standardGeneric('settings'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases settings
#'
setMethod('settings', 'AcousticEvent', function(x, ...) x@settings)

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('settings<-', function(x, value) standardGeneric('settings<-'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases settings
#'
setMethod('settings<-', 'AcousticEvent', function(x, value) {
    x@settings <- value
    validObject(x)
    x
})

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('localizations', function(x, ...) standardGeneric('localizations'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases localizations
#'
setMethod('localizations', 'AcousticEvent', function(x, ...) x@localizations)

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('localizations<-', function(x, value) standardGeneric('localizations<-'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases localizations
#'
setMethod('localizations<-', 'AcousticEvent', function(x, value) {
    x@localizations <- value
    validObject(x)
    x
})

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('id', function(x, ...) standardGeneric('id'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases id
#'
setMethod('id', 'AcousticEvent', function(x, ...) {
    if(inherits(try(x@id, silent=TRUE), 'try-error')) {
        warning('AcousticEvent object does not have an "id" slot (added v0.6.0 Aug-8-2019)',
                '\nPlease see ?setIdSlot to fix this.')
        'SEE ?setIdSlot'
    } else {
        x@id
    }
}
)

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('id<-', function(x, value) standardGeneric('id<-'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases id
#'
setMethod('id<-', 'AcousticEvent', function(x, value) {
    x@id <- value
    validObject(x)
    x
})

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('detectors', function(x, ...) standardGeneric('detectors'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases detectors
#'
setMethod('detectors', 'AcousticEvent', function(x, ...) x@detectors)

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('detectors<-', function(x, value) standardGeneric('detectors<-'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases detectors
#'
setMethod('detectors<-', 'AcousticEvent', function(x, value) {
    x@detectors <- value
    validObject(x)
    x
})

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('visData', function(x, ...) standardGeneric('visData'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases visData
#'
setMethod('visData', 'AcousticEvent', function(x, ...) x@visData)

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('visData<-', function(x, value) standardGeneric('visData<-'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases visData
#'
setMethod('visData<-', 'AcousticEvent', function(x, value) {
    x@visData <- value
    validObject(x)
    x
})

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('behavior', function(x, ...) standardGeneric('behavior'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases behavior
#'
setMethod('behavior', 'AcousticEvent', function(x, ...) x@behavior)

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('behavior<-', function(x, value) standardGeneric('behavior<-'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases behavior
#'
setMethod('behavior<-', 'AcousticEvent', function(x, value) {
    x@behavior <- value
    validObject(x)
    x
})

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('erddap', function(x, ...) standardGeneric('erddap'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases erddap
#'
setMethod('erddap', 'AcousticEvent', function(x, ...) x@erddap)

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('erddap<-', function(x, value) standardGeneric('erddap<-'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases erddap
#'
setMethod('erddap<-', 'AcousticEvent', function(x, value) {
    x@erddap <- value
    validObject(x)
    x
})

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('species', function(x, ...) standardGeneric('species'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases species
#'
setMethod('species', 'AcousticEvent', function(x, ...) x@species)

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('species<-', function(x, value) standardGeneric('species<-'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases species
#'
setMethod('species<-', 'AcousticEvent', function(x, value) {
    x@species <- value
    validObject(x)
    x
})

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('files', function(x, ...) standardGeneric('files'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases files
#'
setMethod('files', 'AcousticEvent', function(x, ...) x@files)

#' @export
#' @rdname AcousticEvent.accessors
#'
setGeneric('files<-', function(x, value) standardGeneric('files<-'))

#' @export
#' @rdname AcousticEvent.accessors
#' @aliases files
#'
setMethod('files<-', 'AcousticEvent', function(x, value) {
    x@files <- value
    validObject(x)
    x
})


#  Get/Set for AcousticStudy class -----------------------------------------------
#' @title \code{AcousticStudy} accessors
#'
#' @description Accessors for slots in \linkS4class{AcousticStudy} objects
#'
#' @param x a \linkS4class{AcousticStudy} object
#' @param value value to assign with accessor
#' @param \dots other arguments to pass to methods
#'
#' @return
#' \describe{
#'   \item{folders}{a list of folders}
#'   \item{gpsData}{a dataframe containing gps data}
#'   \item{acousticEvents}{a list of \linkS4class{AcousticEvent} objects}
#'   \item{detectorSettings}{a named list of settings for each detector used}
#'   \item{localizationSettings}{a named list of settings for each localization}
#'   \item{effort}{something about effort?}
#'   }
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @name AcousticStudy.accessors
#'
#' @importFrom methods setGeneric setMethod validObject
#'
NULL

#' @export
#' @rdname AcousticStudy.accessors
#'
setGeneric('folders', function(x, ...) standardGeneric('folders'))

#' @export
#' @rdname AcousticStudy.accessors
#' @aliases folders
#'
setMethod('folders', 'AcousticStudy', function(x, ...) x@folders)

#' @export
#' @rdname AcousticStudy.accessors
#'
setGeneric('folders<-', function(x, value) standardGeneric('folders<-'))

#' @export
#' @rdname AcousticStudy.accessors
#' @aliases folders
#'
setMethod('folders<-', 'AcousticStudy', function(x, value) {
    x@folders <- value
    validObject(x)
    x
})

#' @export
#' @rdname AcousticStudy.accessors
#'
setGeneric('gpsData', function(x, ...) standardGeneric('gpsData'))

#' @export
#' @rdname AcousticStudy.accessors
#' @aliases gpsData
#'
setMethod('gpsData', 'AcousticStudy', function(x, ...) x@gpsData)

#' @export
#' @rdname AcousticStudy.accessors
#'
setGeneric('gpsData<-', function(x, value) standardGeneric('gpsData<-'))

#' @export
#' @rdname AcousticStudy.accessors
#' @aliases gpsData
#'
setMethod('gpsData<-', 'AcousticStudy', function(x, value) {
    x@gpsData <- value
    validObject(x)
    x
})

#' @export
#' @rdname AcousticStudy.accessors
#'
setGeneric('acousticEvents', function(x, ...) standardGeneric('acousticEvents'))

#' @export
#' @rdname AcousticStudy.accessors
#' @aliases acousticEvents
#'
setMethod('acousticEvents', 'AcousticStudy', function(x, ...) x@acousticEvents)

#' @export
#' @rdname AcousticStudy.accessors
#'
setGeneric('acousticEvents<-', function(x, value) standardGeneric('acousticEvents<-'))

#' @export
#' @rdname AcousticStudy.accessors
#' @aliases acousticEvents
#'
setMethod('acousticEvents<-', 'AcousticStudy', function(x, value) {
    x@acousticEvents <- value
    validObject(x)
    x
})

#' @export
#' @rdname AcousticStudy.accessors
#'
setGeneric('detectorSettings', function(x, ...) standardGeneric('detectorSettings'))

#' @export
#' @rdname AcousticStudy.accessors
#' @aliases detectorSettings
#'
setMethod('detectorSettings', 'AcousticStudy', function(x, ...) x@detectorSettings)

#' @export
#' @rdname AcousticStudy.accessors
#'
setGeneric('detectorSettings<-', function(x, value) standardGeneric('detectorSettings<-'))

#' @export
#' @rdname AcousticStudy.accessors
#' @aliases detectorSettings
#'
setMethod('detectorSettings<-', 'AcousticStudy', function(x, value) {
    x@detectorSettings <- value
    validObject(x)
    x
})

#' @export
#' @rdname AcousticStudy.accessors
#'
setGeneric('localizationSettings', function(x, ...) standardGeneric('localizationSettings'))

#' @export
#' @rdname AcousticStudy.accessors
#' @aliases localizationSettings
#'
setMethod('localizationSettings', 'AcousticStudy', function(x, ...) x@localizationSettings)

#' @export
#' @rdname AcousticStudy.accessors
#'
setGeneric('localizationSettings<-', function(x, value) standardGeneric('localizationSettings<-'))

#' @export
#' @rdname AcousticStudy.accessors
#' @aliases localizationSettings
#'
setMethod('localizationSettings<-', 'AcousticStudy', function(x, value) {
    x@localizationSettings <- value
    validObject(x)
    x
})

#' @export
#' @rdname AcousticStudy.accessors
#'
setGeneric('effort', function(x, ...) standardGeneric('effort'))

#' @export
#' @rdname AcousticStudy.accessors
#' @aliases effort
#'
setMethod('effort', 'AcousticStudy', function(x, ...) x@effort)

#' @export
#' @rdname AcousticStudy.accessors
#'
setGeneric('effort<-', function(x, value) standardGeneric('effort<-'))

#' @export
#' @rdname AcousticStudy.accessors
#' @aliases effort
#'
setMethod('effort<-', 'AcousticStudy', function(x, value) {
    x@effort <- value
    validObject(x)
    x
})

#  Get/Set for DataSettings class -----------------------------------------------
#' @title \code{DataSettings} accessors
#'
#' @description Accessors for slots in \linkS4class{DataSettings} objects
#'
#' @param x a \linkS4class{DataSettings} object
#' @param value value to assign with accessor
#' @param \dots other arguments to pass to methods
#'
#' @return
#' \describe{
#'   \item{sampleRate}{sample rate in Hz of the sound}
#'   \item{soundSource}{source of the sound}
#'   }
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @name DataSettings.accessors
#'
#' @importFrom methods setGeneric setMethod validObject
#'
NULL

#' @export
#' @rdname DataSettings.accessors
#'
setGeneric('sampleRate', function(x, ...) standardGeneric('sampleRate'))

#' @export
#' @rdname DataSettings.accessors
#' @aliases sampleRate
#'
setMethod('sampleRate', 'DataSettings', function(x, ...) x@sampleRate)

#' @export
#' @rdname DataSettings.accessors
#'
setGeneric('sampleRate<-', function(x, value) standardGeneric('sampleRate<-'))

#' @export
#' @rdname DataSettings.accessors
#' @aliases sampleRate
#'
setMethod('sampleRate<-', 'DataSettings', function(x, value) {
    x@sampleRate <- as.integer(value)
    validObject(x)
    x
})

#' @export
#' @rdname DataSettings.accessors
#'
setGeneric('soundSource', function(x, ...) standardGeneric('soundSource'))

#' @export
#' @rdname DataSettings.accessors
#' @aliases soundSource
#'
setMethod('soundSource', 'DataSettings', function(x, ...) x@soundSource)

#' @export
#' @rdname DataSettings.accessors
#'
setGeneric('soundSource<-', function(x, value) standardGeneric('soundSource<-'))

#' @export
#' @rdname DataSettings.accessors
#' @aliases soundSource
#'
setMethod('soundSource<-', 'DataSettings', function(x, value) {
    x@soundSource <- value
    validObject(x)
    x
})
