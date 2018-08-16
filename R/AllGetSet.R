# All get/set

# Get/Set for AcousticEvent class -----------------------------------------

setGeneric('settings', function(x, ...) standardGeneric('settings'))
setMethod('settings', 'AcousticEvent', function(x, ...) x@settings)
setGeneric('settings<-', function(x, value) standardGeneric('settings<-'))
setMethod('settings<-', 'AcousticEvent', function(x, value) {
  x@settings <- value
  validObject(x)
  x
})

setGeneric('localizations', function(x, ...) standardGeneric('localizations'))
setMethod('localizations', 'AcousticEvent', function(x, ...) x@localizations)
setGeneric('localizations<-', function(x, value) standardGeneric('localizations<-'))
setMethod('localizations<-', 'AcousticEvent', function(x, value) {
  x@localizations <- value
  validObject(x)
  x
})

setGeneric('detectors', function(x, ...) standardGeneric('detectors'))
setMethod('detectors', 'AcousticEvent', function(x, ...) x@detectors)
setGeneric('detectors<-', function(x, value) standardGeneric('detectors<-'))
setMethod('detectors<-', 'AcousticEvent', function(x, value) {
  x@detectors <- value
  validObject(x)
  x
})

setGeneric('visData', function(x, ...) standardGeneric('visData'))
setMethod('visData', 'AcousticEvent', function(x, ...) x@visData)
setGeneric('visData<-', function(x, value) standardGeneric('visData<-'))
setMethod('visData<-', 'AcousticEvent', function(x, value) {
  x@visData <- value
  validObject(x)
  x
})

setGeneric('behavior', function(x, ...) standardGeneric('behavior'))
setMethod('behavior', 'AcousticEvent', function(x, ...) x@behavior)
setGeneric('behavior<-', function(x, value) standardGeneric('behavior<-'))
setMethod('behavior<-', 'AcousticEvent', function(x, value) {
  x@behavior <- value
  validObject(x)
  x
})

setGeneric('erdap', function(x, ...) standardGeneric('erdap'))
setMethod('erdap', 'AcousticEvent', function(x, ...) x@erdap)
setGeneric('erdap<-', function(x, value) standardGeneric('erdap<-'))
setMethod('erdap<-', 'AcousticEvent', function(x, value) {
  x@erdap <- value
  validObject(x)
  x
})

setGeneric('specClass', function(x, ...) standardGeneric('specClass'))
setMethod('specClass', 'AcousticEvent', function(x, ...) x@specClass)
setGeneric('specClass<-', function(x, value) standardGeneric('specClass<-'))
setMethod('specClass<-', 'AcousticEvent', function(x, value) {
  x@specClass <- value
  validObject(x)
  x
})

setGeneric('files', function(x, ...) standardGeneric('files'))
setMethod('files', 'AcousticEvent', function(x, ...) x@files)
setGeneric('files<-', function(x, value) standardGeneric('files<-'))
setMethod('files<-', 'AcousticEvent', function(x) {
  x@files <- value
  validObject(x)
  x
})


#  Get/Set for Cruise class -----------------------------------------------

setGeneric('folders', function(x, ...) standardGeneric('folders'))
setMethod('folders', 'Cruise', function(x, ...) x@folders)
setGeneric('folders<-', function(x, value) standardGeneric('folders<-'))
setMethod('folders<-', 'Cruise', function(x, value) {
  x@folders <- value
  validObject(x)
  x
})

setGeneric('gpsData', function(x, ...) standardGeneric('gpsData'))
setMethod('gpsData', 'Cruise', function(x, ...) x@gpsData)
setGeneric('gpsData<-', function(x, value) standardGeneric('gpsData<-'))
setMethod('gpsData<-', 'Cruise', function(x, value) {
  x@gpsData <- value
  validObject(x)
  x
})

setGeneric('acousticEvents', function(x, ...) standardGeneric('acousticEvents'))
setMethod('acousticEvents', 'Cruise', function(x, ...) x@acousticEvents)
setGeneric('acousticEvents<-', function(x, value) standardGeneric('acousticEvents<-'))
setMethod('acousticEvents<-', 'Cruise', function(x, value) {
  x@acousticEvents <- value
  validObject(x)
  x
})

setGeneric('detectorSettings', function(x, ...) standardGeneric('detectorSettings'))
setMethod('detectorSettings', 'Cruise', function(x, ...) x@detectorSettings)
setGeneric('detectorSettings<-', function(x, value) standardGeneric('detectorSettings<-'))
setMethod('detectorSettings<-', 'Cruise', function(x, value) {
  x@detectorSettings <- value
  validObject(x)
  x
})

setGeneric('localizationSettings', function(x, ...) standardGeneric('localizationSettings'))
setMethod('localizationSettings', 'Cruise', function(x, ...) x@localizationSettings)
setGeneric('localizationSettings<-', function(x, value) standardGeneric('localizationSettings<-'))
setMethod('localizationSettings<-', 'Cruise', function(x, value) {
  x@localizationSettings <- value
  validObject(x)
  x
})

setGeneric('effort', function(x, ...) standardGeneric('effort'))
setMethod('effort', 'Cruise', function(x, ...) x@effort)
setGeneric('effort<-', function(x, value) standardGeneric('effort<-'))
setMethod('effort<-', 'Cruise', function(x, value) {
  x@effort <- value
  validObject(x)
  x
})

#  Get/Set for DataSettings class -----------------------------------------------

setGeneric('sampleRate', function(x, ...) standardGeneric('sampleRate'))
setMethod('sampleRate', 'DataSettings', function(x, ...) x@sampleRate)
setGeneric('sampleRate<-', function(x, value) standardGeneric('sampleRate<-'))
setMethod('sampleRate<-', 'DataSettings', function(x, value) {
  x@sampleRate <- as.integer(value)
  validObject(x)
  x
})
setGeneric('soundSource', function(x, ...) standardGeneric('soundSource'))
setMethod('soundSource', 'DataSettings', function(x, ...) x@soundSource)
setGeneric('soundSource<-', function(x, value) standardGeneric('soundSource<-'))
setMethod('soundSource<-', 'DataSettings', function(x, value) {
  x@soundSource <- value
  validObject(x)
  x
})



