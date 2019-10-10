## ---- PAMrSettings Class ----------------------------------------------------
#' @title \code{PAMrSettings} Class
#' @description An S4 class that stores settings related to all processing and analysis steps
#' done in PAMr. A PAMrSettings object will be the main input to any major function
#' in the PAMr package.
#'
#' @slot db the full path to a PamGuard database file
#' @slot binaries a list with items "folder" containing the directory of the
#'   PamGuard binary files, and "list" containing the full path to each individual
#'   binary file.
#' @slot functions a named list of functions to apply to data read in by PAMr.
#'   Should be named by the PamGuard module the function should be applied to.
#'   Currently supports "ClickDetector", "WhistlesMoans", and "Cepstrum".
#' @slot calibration a named list of calibration functions to apply while
#'   applying functions from the "functions" slot. Should named by the
#'   PamGuard module, same as the "functions"
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' @export
#'
setClass('PAMrSettings',
         slots = c(
             db = 'character',
             binaries = 'list',
             functions = 'list',
             calibration = 'list'
         ),
         prototype = prototype(
             db = character(0),
             binaries = list('folder'=character(0), 'list'=character(0)),
             functions = list('ClickDetector'=list(), 'WhistlesMoans'=list(), 'Cepstrum'=list()),
             calibration = list('ClickDetector'=list())
         )
)

setValidity('PAMrSettings',
            function(object) {
                valid <- TRUE
                if(!all(c('folder', 'list') %in% names(object@binaries))) {
                    valid <- FALSE
                    cat('slot binaries must have items "folder" and "list"\n')
                }
                if(!all(c('ClickDetector', 'WhistlesMoans', 'Cepstrum') %in% names(object@functions))) {
                    valid <- FALSE
                    cat('slot functions must have items "ClickDetector", "WhistlesMoans", and "Cepstrum"\n')
                }
                valid
            }
)

#' @importFrom utils str
#'
setMethod('show', 'PAMrSettings', function(object) {
    nBin <- length(object@binaries$list)
    nBinDir <- length(object@binaries$folder)
    nDb <- length(object@db)
    nCal <- length(object@calibration$ClickDetector)
    cat('PAMrSettings object with:\n')
    cat(nDb, 'database(s)')
    if(nDb > 0) {
        showDb <- basename(object@db)
        if(nDb > 6) {
            showDb <- c(showDb[1:6], paste0(nDb-6, ' more not shown ...'))
        }
        cat(':\n ', paste(showDb, collapse='\n  '))
    }
    cat('\n', nBinDir, ' binary folder(s) ', sep = '')
    if(nBinDir > 0) {
        cat('containing', nBin, 'binary files\n')
    } else {
        cat('\n')
    }
    # Print function names and args for each module
    for(m in seq_along(object@functions)) {
        cat(length(object@functions[[m]]), ' function(s) for module type "',
            names(object@functions)[m], '"\n', sep = '')
        for(f in seq_along(object@functions[[m]])) {
            cat(' "', names(object@functions[[m]])[f], '"\n  ', sep = '')
            cat(str(object@functions[[m]][[f]]))
        }
    }
    cat(nCal, 'click calibration function(s)\n')
})

## ---- DataSettings Class ----------------------------------------------------
# Data Collection / Array Settings (obj)              \\settings
# Hydro sens, sample rate, whatever. Make an object and we figure out what it needs
#' An S4 class to store data collection settings. Possible inclusions are
#' hydrophone sensitivity, sample rate, sound card, etc.
#'
#' @slot sampleRate the sample rate data was recorded at.
#' @slot soundSource the source of the recorded sound - sound card, recording
#'   system, or sound file
#'
setClass('DataSettings',
         slots = c(
             sampleRate = 'integer',
             soundSource = 'character'
         ),
         prototype = prototype(sampleRate=NA_integer_, soundSource='Not Found')
)

setValidity('DataSettings',
            function(object) {
                TRUE
            }
)

DataSettings <- function(sampleRate=NA_integer_, soundSource='Not Found') {
    if(missing(sampleRate)) {
        warning('"sampleRate" not specified.')
    }
    if(missing(soundSource)) {
        # warning('"soundSource" not found.')
    }
    new('DataSettings', sampleRate=as.integer(sampleRate), soundSource=soundSource)
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

#' @title \code{VisObsData} Class
#' @description An S4 class storing visual obsever data (UNFINISHED)
#'
#' @slot detectionTime detection time
#' @slot speciesId species id
#' @slot groupSizeEst group size estimate
#' @slot effortStatus effort status
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' @export
#'
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
# erddap                                               \\erddap
# https://github.com/rmendels/Talks/blob/master/netCDF_Presentation/netcdf_opendap_erddap.Rmd
# Species classification - list of classifier objects \\species
# Method, prediction, assignment probabilities
# Duration? Files used? ID?

# setClassUnion('VisOrNULL', c('VisObsData', 'NULL'))

#' @title \code{AcousticEvent} Class
#' @description An S4 class storing acoustic detections from an Acoustic Event
#'   as well as other related metadata
#'
#' @slot id unique id or name for this event
#' @slot detectors a list of data frames that have acoustic detections and
#'   any measurements calculated on those detections. Each data frame is named
#'   by the detector that made the detection
#' @slot localizations a named list storing localizations, named by method
#' @slot settings a \linkS4class{DataSettings} object for this event
#' @slot visData a \linkS4class{VisObsData} with visual data for this event
#' @slot behavior behavior data
#' @slot erddap environmental data
#' @slot species a list of species classifications for this event, named by
#'   classification method (ie. BANTER model, visual ID)
#' @slot files a list of files used to create this object, named by the type of
#'   file (ie. binaries, database)
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' @export
#'
setClass('AcousticEvent',
         slots = c(
             id = 'character',
             detectors = 'list',
             localizations = 'list',
             settings = 'DataSettings',
             visData = 'VisObsData',
             behavior = 'list',
             erddap = 'list',
             species = 'list',
             files = 'list'),
         prototype = prototype(id = character(), detectors=list(), localizations=list(), settings=DataSettings(),
                               visData=VisObsData(), behavior=list(), erddap=list(), species=list(id=NA_character_),
                               files = list())
)

setValidity('AcousticEvent',
            function(object) {
                valid <- TRUE
                valid
            }
)
# Basic constructor
AcousticEvent <- function(id = character(), detectors=list(), localizations=list(), settings=DataSettings(), visData=VisObsData(),
                          behavior=list(), erddap=list(), species=list(id=NA_character_), files=list()) {
    new('AcousticEvent', id = id, detectors=detectors, localizations=localizations, settings=settings,
        visData=visData, behavior=behavior, erddap=erddap, species=species, files=files)
}

setMethod('show', 'AcousticEvent',
          function(object) {
              cat('AcousticEvent object "', id(object), '" with ',
                  length(object@detectors), ' detector(s): \n', sep='')
              cat(paste(names(object@detectors), collapse=', '))
          }
)

## ---- AcousticStudy Class ----------------------------------------------------------
# AcousticStudy class
# AcousticStudy (object)
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
# erddap                                               \\erddap
# Species classification - list of classifier objects \\species
# Method, prediction, assignment probabilities
# Detector settings - named list [[detector name]]   \detectorSettings
# Localization settings - named list [[ loc. type]]  \localizationSettings
# Some effort bullshit                               \effort
# ??????


#' @title \code{AcousticStudy} Class
#' @description An S4 class storing acoustic data from an entire AcousticStudy
#' @slot id a unique id for the study
#' @slot files a list of folders and files containing the AcousticStudy data
#' @slot gpsData a data frame of gps coordinates for the entire AcousticStudy
#' @slot events a list of \linkS4class{AcousticEvent} objects with
#'   detections from the AcousticStudy
#' @slot prs the \linkS4class{PAMrSettings} object used to create this object
#' @slot settings a named list of various settings for detectors, localizers, etc.
#' @slot effort something about effort lol
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' @export
#'
setClass('AcousticStudy',
         slots = c(
             id = 'character',
             files = 'list',
             gpsData = 'data.frame',
             events = 'list',
             prs = 'PAMrSettings',
             settings = 'list',
             effort = 'data.frame'), # maybe
         prototype = prototype(
             id = character(),
             files=list(database='None', binaries='None', visual='None', enviro='None'),
             gpsData=data.frame(), events=list(), settings=list(),
             prs = PAMrSettings(), effort=data.frame())
)

setValidity('AcousticStudy',
            function(object) {
                valid <- TRUE
                valid
            }
)

# Constructor
AcousticStudy <- function(folders=list(datbase='None', binaries='None', visData='None', enviroData='None'),
                   gpsData=data.frame(), acousticEvents=list(), detectorSettings=list(),
                   localizationSettings=list(), effort=data.frame()) {
    new('AcousticStudy', folders=folders, gpsData=gpsData, acousticEvents=acousticEvents,
        detectorSettings=detectorSettings, localizationSettings=localizationSettings, effort=effort)
}
