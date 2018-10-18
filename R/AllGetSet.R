# All get/set
# Get/Set for AcousticEvent class -----------------------------------------

#' @export
setGeneric('settings', function(x, ...) standardGeneric('settings'))

#' @export
setMethod('settings', 'AcousticEvent', function(x, ...) x@settings)

#' @export
setGeneric('settings<-', function(x, value) standardGeneric('settings<-'))

#' @importFrom methods validObject
#' @export
setMethod('settings<-', 'AcousticEvent', function(x, value) {
    x@settings <- value
    validObject(x)
    x
})

#' @export
setGeneric('localizations', function(x, ...) standardGeneric('localizations'))

#' @export
setMethod('localizations', 'AcousticEvent', function(x, ...) x@localizations)

#' @export
setGeneric('localizations<-', function(x, value) standardGeneric('localizations<-'))

#' @export
setMethod('localizations<-', 'AcousticEvent', function(x, value) {
    x@localizations <- value
    validObject(x)
    x
})

#' @export
setGeneric('detectors', function(x, ...) standardGeneric('detectors'))

#' @export
setMethod('detectors', 'AcousticEvent', function(x, ...) x@detectors)

#' @export
setGeneric('detectors<-', function(x, value) standardGeneric('detectors<-'))

#' @export
setMethod('detectors<-', 'AcousticEvent', function(x, value) {
    x@detectors <- value
    validObject(x)
    x
})

#' @export
setGeneric('visData', function(x, ...) standardGeneric('visData'))

#' @export
setMethod('visData', 'AcousticEvent', function(x, ...) x@visData)

#' @export
setGeneric('visData<-', function(x, value) standardGeneric('visData<-'))

#' @export
setMethod('visData<-', 'AcousticEvent', function(x, value) {
    x@visData <- value
    validObject(x)
    x
})

#' @export
setGeneric('behavior', function(x, ...) standardGeneric('behavior'))

#' @export
setMethod('behavior', 'AcousticEvent', function(x, ...) x@behavior)

#' @export
setGeneric('behavior<-', function(x, value) standardGeneric('behavior<-'))

#' @export
setMethod('behavior<-', 'AcousticEvent', function(x, value) {
    x@behavior <- value
    validObject(x)
    x
})

#' @export
setGeneric('erddap', function(x, ...) standardGeneric('erddap'))

#' @export
setMethod('erddap', 'AcousticEvent', function(x, ...) x@erddap)

#' @export
setGeneric('erddap<-', function(x, value) standardGeneric('erddap<-'))

#' @export
setMethod('erddap<-', 'AcousticEvent', function(x, value) {
    x@erddap <- value
    validObject(x)
    x
})

#' @export
setGeneric('specClass', function(x, ...) standardGeneric('specClass'))

#' @export
setMethod('specClass', 'AcousticEvent', function(x, ...) x@specClass)

#' @export
setGeneric('specClass<-', function(x, value) standardGeneric('specClass<-'))

#' @export
setMethod('specClass<-', 'AcousticEvent', function(x, value) {
    x@specClass <- value
    validObject(x)
    x
})

#' @export
setGeneric('files', function(x, ...) standardGeneric('files'))

#' @export
setMethod('files', 'AcousticEvent', function(x, ...) x@files)

#' @export
setGeneric('files<-', function(x, value) standardGeneric('files<-'))

#' @export
setMethod('files<-', 'AcousticEvent', function(x, value) {
    x@files <- value
    validObject(x)
    x
})


#  Get/Set for Cruise class -----------------------------------------------

#' @export
setGeneric('folders', function(x, ...) standardGeneric('folders'))

#' @export
setMethod('folders', 'Cruise', function(x, ...) x@folders)

#' @export
setGeneric('folders<-', function(x, value) standardGeneric('folders<-'))

#' @export
setMethod('folders<-', 'Cruise', function(x, value) {
    x@folders <- value
    validObject(x)
    x
})

#' @export
setGeneric('gpsData', function(x, ...) standardGeneric('gpsData'))

#' @export
setMethod('gpsData', 'Cruise', function(x, ...) x@gpsData)

#' @export
setGeneric('gpsData<-', function(x, value) standardGeneric('gpsData<-'))

#' @export
setMethod('gpsData<-', 'Cruise', function(x, value) {
    x@gpsData <- value
    validObject(x)
    x
})

#' @export
setGeneric('acousticEvents', function(x, ...) standardGeneric('acousticEvents'))

#' @export
setMethod('acousticEvents', 'Cruise', function(x, ...) x@acousticEvents)

#' @export
setGeneric('acousticEvents<-', function(x, value) standardGeneric('acousticEvents<-'))

#' @export
setMethod('acousticEvents<-', 'Cruise', function(x, value) {
    x@acousticEvents <- value
    validObject(x)
    x
})

#' @export
setGeneric('detectorSettings', function(x, ...) standardGeneric('detectorSettings'))

#' @export
setMethod('detectorSettings', 'Cruise', function(x, ...) x@detectorSettings)

#' @export
setGeneric('detectorSettings<-', function(x, value) standardGeneric('detectorSettings<-'))

#' @export
setMethod('detectorSettings<-', 'Cruise', function(x, value) {
    x@detectorSettings <- value
    validObject(x)
    x
})

#' @export
setGeneric('localizationSettings', function(x, ...) standardGeneric('localizationSettings'))

#' @export
setMethod('localizationSettings', 'Cruise', function(x, ...) x@localizationSettings)

#' @export
setGeneric('localizationSettings<-', function(x, value) standardGeneric('localizationSettings<-'))

#' @export
setMethod('localizationSettings<-', 'Cruise', function(x, value) {
    x@localizationSettings <- value
    validObject(x)
    x
})

#' @export
setGeneric('effort', function(x, ...) standardGeneric('effort'))

#' @export
setMethod('effort', 'Cruise', function(x, ...) x@effort)

#' @export
setGeneric('effort<-', function(x, value) standardGeneric('effort<-'))

#' @export
setMethod('effort<-', 'Cruise', function(x, value) {
    x@effort <- value
    validObject(x)
    x
})

#  Get/Set for DataSettings class -----------------------------------------------

#' @export
setGeneric('sampleRate', function(x, ...) standardGeneric('sampleRate'))

#' @export
setMethod('sampleRate', 'DataSettings', function(x, ...) x@sampleRate)

#' @export
setGeneric('sampleRate<-', function(x, value) standardGeneric('sampleRate<-'))

#' @export
setMethod('sampleRate<-', 'DataSettings', function(x, value) {
    x@sampleRate <- as.integer(value)
    validObject(x)
    x
})

#' @export
setGeneric('soundSource', function(x, ...) standardGeneric('soundSource'))

#' @export
setMethod('soundSource', 'DataSettings', function(x, ...) x@soundSource)

#' @export
setGeneric('soundSource<-', function(x, value) standardGeneric('soundSource<-'))

#' @export
setMethod('soundSource<-', 'DataSettings', function(x, value) {
    x@soundSource <- value
    validObject(x)
    x
})
