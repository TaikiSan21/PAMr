## ---- DataSettings Class ----------------------------------------------------

# Data Collection / Array Settings (obj)              \\settings
# Hydro sens, sample rate, whatever. Make an object and we figure out what it needs

setClass('DataSettings',
         slots = c(
           sampleRate = 'integer',
           soundSource = 'character',
           otherStuff = 'list'
         ),
         prototype = prototype(sampleRate=192e3L, soundSource='Not Found', otherStuff=list())
)

setValidity('DataSettings',
            function(object) {
              TRUE
            }
)

DataSettings <- function(sampleRate=192e3L, soundSource='Not Found', otherStuff=list()) {
  if(missing(sampleRate)) {
    warning('"sampleRate" not specified, default is 192000Hz')
  }
  if(missing(soundSource)) {
    warning('"soundSource" not found.')
  }
  new('DataSettings', sampleRate=as.integer(sampleRate), soundSource=soundSource, otherStuff=otherStuff)
}

setMethod('show', 'DataSettings',
          function(object) {
            sampleRates <- object@sampleRate
            soundSources <- object@soundSource
            if(length(sampleRates) > 6) {
              sampleRates <- c(sampleRates[1:6], '...')
            }
            if(length(soundSources) > 6) {
              soundSources <- c(soundSources[1:6], '...')
            }
            sampleRates <- paste(sampleRates, collapse=', ')
            soundSources <- paste(soundSources, collapse=', ')
            cat('DataSettings object with settings:\nSample Rate(s):', sampleRates,
                '\nSound Source(s):', soundSources)
          }
)


# Were gonna get sampleRate and soundcard system type from SoundAcquisition table sampleRate and SystemType
# Other stuff from a logger form? Iffy..for HICEAS this is split across different fuckiNG DATBASES

## ---- VisObsData Class ------------------------------------------------------
# Visual data (obj)                                   \\visData
# Detection time, spp IDs, group size est, effort status. Multiple ways to read

setClass('VisObsData',
         slots = c(
           detectionTime = 'POSIXct',
           speciesId = 'character',
           groupSizeEst = 'numeric',
           effortStatus = 'character'
         ),
         prototype = prototype(detectionTime = Sys.time(), speciesId = 'None',
                               groupSizeEst = NaN, effortStatus = 'None')
)

setValidity('VisObsData',
            function(object) {
              TRUE
            }
)

# Basic constructor
VisObsData <- function(detectionTime=Sys.time(), speciesId='None',
                       groupSizeEst=NaN, effortStatus='None') {
  new('VisObsData', detectionTime=detectionTime, speciesId=speciesId,
      groupSizeEst=groupSizeEst, effortStatus=effortStatus)
}

## ---- AcousticEvent Class ---------------------------------------------------

# Acoustic event (obj) <--- this is really a list of AcEv? These need an ID for banter \acousticEvents
# Detector - named list [[detector name]] of lists    \\detector
# Data.table of detections w/ id
# possible image
# Localization - named list[[loc. type name]]         \\localization
# Data frame of positions
# Data Collection / Array Settings (obj)              \\settings
# Hydro sens, sample rate, whatever. Make an object and we figure out what it needs
# Visual data (obj)                                   \\visData
# Detection time, spp IDs, group size est, effort status. Multiple ways to read
# Behavioral (lul)                                    \\behavior
# ERDAP                                               \\erdap
# https://github.com/rmendels/Talks/blob/master/netCDF_Presentation/netcdf_opendap_erddap.Rmd
# Species classification - list of classifier objects \\specClass
# Method, prediction, assignment probabilities
# Duration? Files used? ID?

# Will want to have an assign species method for these. When going from PAMr -> training BANTER
# need a way to mark species by event outside of PAMGuard.
# Could have option to do from OfflineEvent comment or eventType field. Should we read these in
# to start? And somewhat hide them? PITA to go back and find them after, tho I guess we are
# possibly going to save the DB info somewhere. Actually we should definitely save that somewhere.
# Have a 'files used' or some shit with DB and all binary files read to make it

# Or are we doing this at cruise level? seems wrong...
# setClassUnion('VisOrNULL', c('VisObsData', 'NULL'))

setClass('AcousticEvent',
         slots = c(
           detectors = 'list',
           localizations = 'list',
           settings = 'DataSettings',
           visData = 'VisObsData',
           behavior = 'list',
           erdap = 'list',
           specClass = 'list',
           files = 'list'),
         prototype = prototype(detectors=list(), localizations=list(), settings=DataSettings(),
                               visData=VisObsData(), behavior=list(), erdap=list(), specClass=list(),
                               files = list())
)

setValidity('AcousticEvent',
            function(object) {
              valid <- TRUE
              if(length(object@detectors)==0) {
                cat('AcousticEvent object must have at least one detector. \n')
                valid <- FALSE
              } else if(is.null(names(object@detectors))) {
                cat('All detectors in the "detectors" slot must be named. \n')
                valid <- FALSE
              }
              valid
            }
)
# Basic constructor
AcousticEvent <- function(detectors=list(), localizations=list(), settings=DataSettings(), visData=VisObsData(),
                          behavior=list(), erdap=list(), specClass=list(), files=list()) {
  new('AcousticEvent', detectors=detectors, localizations=localizations, settings=settings,
      visData=visData, behavior=behavior, erdap=erdap, specClass=specClass, files=files)
}

setMethod('show', 'AcousticEvent',
          function(object) {
            cat('AcousticEvent object with', length(object@detectors), 'detector(s): \n')
            cat(paste(names(object@detectors), collapse=', '))
          }
)


## ---- Cruise Class ----------------------------------------------------------
# Cruise class
# Cruise (object)
# Files / folders (dbs, bins, vis, enviro)      \folders
# GPS                                           \gpsData
# Acoustic event (obj) <--- this is really a list of AcEv? These need an ID for banter \acousticEvents
# Detector - named list [[detector name]] of lists    \\detector
# Data.table of detections w/ id
# possible image
# Localization - named list[[loc. type name]]         \\localization
# Data frame of positions
# Data Collection / Array Settings (obj)              \\settings
# Hydro sens, sample rate, whatever. Make an object and we figure out what it needs
# Visual data (obj)                                   \\visData
# Detection time, spp IDs, group size est, effort status. Multiple ways to read
# Behavioral (lul)                                    \\behavior
# ERDAP                                               \\erdap
# Species classification - list of classifier objects \\specClass
# Method, prediction, assignment probabilities
# Detector settings - named list [[detector name]]   \detectorSettings
# Localization settings - named list [[ loc. type]]  \localizationSettings
# Some effort bullshit                               \effort
# ??????
# ???????

setClass('Cruise',
         slots = c(
           folders = 'list',
           gpsData = 'data.frame',
           acousticEvents = 'list',
           detectorSettings = 'list',
           localizationSettings = 'list',
           effort = 'data.frame'), # maybe
         prototype = prototype(
           folders=list(database='None', binaries='None', visData='None', enviroData='None'),
           gpsData=data.frame(), acousticEvents=list(), detectorSettings=list(),
           localizationSettings=list(), effort=data.frame())
)

setValidity('Cruise',
            function(object) {
              valid <- TRUE
              # This doesnt work if there are none. Required to have some or not?
              if(!all(sapply(object@acousticEvents, function(x) class(x)=='AcousticEvent'))) {
                cat('Slot acousticEvents must be a list of AcousticEvent objects. \n')
                valid <- FALSE
              }
              if(!all(names(object@folders) %in% c('database', 'binaries', 'visData', 'enviroData'))) {
                cat('Slot folders must be a list with names "database", "binaries", "visData", and "enviroData". \n')
                valid <- FALSE
              }
              # check all detecotrs in acevs are in detectorSettings list
              valid
            }
)

# Constructor
Cruise <- function(folders=list(datbase='None', binaries='None', visData='None', enviroData='None'),
                   gpsData=data.frame(), acousticEvents=list(), detectorSettings=list(),
                   localizationSettings=list(), effort=data.frame()) {
  new('Cruise', folders=folders, gpsData=gpsData, acousticEvents=acousticEvents,
      detectorSettings=detectorSettings, localizationSettings=localizationSettings, effort=effort)
}
