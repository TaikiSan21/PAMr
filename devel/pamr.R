# PAMR OUTLINE
# Survey
  # Cruise (object)
    # Files / folders (dbs, bins, vis, enviro)   MAYBE this is just a function that aggregates from AcEvs
    # GPS
    # Acoustic event (obj) <--- this is really a list of AcEv?
      # Detector - named list [[detector name]] of lists
        # Data.table of detections w/ id
        # possible image
      # Localization - named list[[loc. type name]]
        # Data frame of positions
      # Data Collection / Array Settings (obj)
        # Hydro sens, sample rate, whatever. Make an object and we figure out what it needs
      # Visual data (obj)
        # Detection time, spp IDs, group size est, effort status. Multiple ways to read
      # Behavioral (lul)
      # ERDAP https://github.com/rmendels/Talks/blob/master/netCDF_Presentation/netcdf_opendap_erddap.Rmd
      # Species classification - list of classifier objects
        # Method, prediction, assignment probabilities
      # Add a thing for duration?
    # Detector settings - named list [[detector name]]
    # Localization settings - named list [[ loc. type]]
    # Some effort bullshit
      # ??????
      # ???????

# from PAMr event -> species ID
# for each kind of detector: indiv detections have UID -> tied to event ID -> that has species ID NO IT DOESNT YOU FUCK
# probably survey name or number for each event - make sure that across seasons we can tell diff


# Offline event id in rocca csv -> eventid in offline clicks table. CSV might be same as rocca whistle stats table? Duration is 0 for clicks tho

## DETCTOR NAMES
# For seeing what detectors we have - PamguardModules table will have a thing for each detector (seems every time its run, or finds something).
# Module_Name has WhistlesMoans and Click Detector, then Module_Type has the name of the detector. These correspond to table names.
# For clicks: Name_of_detector_(Background/Clicks/OfflineClicks/OfflineEvents)
# For WM: Name_of_detector and Name_of_detector_Localised_Name_of_detector_Contours
## BINARY names
# For clicks: Click_Detector_Name_of_detector_(Clicks/Trigger_Background)_YYYMMDD_HHMMSS.pgdf
# WM: WhistlesMoans_Name_of_detector_Contours_YYYMMDD_HHMMSS.pgdf
## ROCCA
# Rocca_Whistle_Stats only has the eventid, no specific UID for calls (has a UID column, but it is wrong. has normal IDs)
# This might be a problem? Or might not since we just want everything anyway? All clicks have an eventid so we can just
# filter down by clicks with event ids in rocca (if they don't match then we have to do shitty matching I guess).
# PROBLEM: Whistles don't seem to have an event ID. I cant get whistles to work in rocca so I have no idea what it looks like.

##### UIDS ARE BUGGED IN: ######
# Clickdet OfflineEvents, Rocca whistle stats, rocca encounter stats

## WAVE: Binary wave form doesnt match that well to wave viewed in raven. Sample no's definitely dont match up right

## BANTER ##
# eventmodel data: event ids, species ids, duration, WHATEVER. Detector level data: only call id, event id, WHATEVER. No species.
# All species in an event are the same.
# Banter model is initialized first with event DB [event.id, species, ...]
# Then we add detectors [event.id, call.id, ...]

#### BANTER POSSIBLE IMPROVEMENTS #####
# We can't add new events to existing model, have to re-init?
# Check call.id and event.id in add detector model or at least add to param description
# More specific error if detector only has 1 species

# Should banter crash completely if NA values? Or warn and ignore?
# Put either total count in summary confusion matrix, or put Prior next to pct.correct

# Summary graph should have consistent y-axis 0-1

# ERDAP LATER:
# https://github.com/rmendels/Talks/blob/master/netCDF_Presentation/netcdf_opendap_erddap.Rmd

## DATA GROUPING:
# Are we going to be using the detection group localiser to do groups / events? If yes,
# can use the Detection_Group_Localiser_Children table to get UIDs and modules that shit
# came from so that we can read data from that motherfucker. It has UID, binaryfile, and module.
# Detection_Group_Localiser has TM localisation result, not sure what else is useful there.
## WE MIGHT DO DET GROUP LOC
# It has annotation field, either Text_Annotation if you chose that, or the DB names will
# be based on whatever form you designed. These field names are saved in UDF_(psfname?) table
### I THINK THIS IS JUST ALL IN OFFLINE CLICKS
# older versions do not have LongDataName field, only binary file. Maybe not a problem,
# can parse out the type of detector and name of detector from bin file name if necessary


# Might need to know what FFT source shit came from? Or FFT settings? No good way to
# get BP detector info right now other than the name of the shit.

# CALIBRATIONS
# Maybe put 'Calibration' in with binFuns, this should probably just have the
# sensitivity by freq sort of calibration file. Can maybe have option of 'gam'
# 'interpolate' whatever, then it will make that model every time and know
# how to predict or whatever from there. OR when we build this shit, have it
# create a function we store there so we can do cal(shit) instead of
# predict.gam(gam, shit) or interp(cals, shit)

# VIS DATA
# I have no idea how we are matching our events to either Acoustic IDs or
# Visual IDs. These are not tied to events. Is this going to be commented
# later?

# TODO:
# We want to keep the 'eventType' from events table probably as it has labels
# that people made. PROBLEM: there is some bug with UIDs, so I dont know best
# way to match these. Id seems fine, but that probably isnt a great way in general
# Just do bespoke version for anne until fix, try to recreate from scratch.
# Add 'try' to the processing things so doesnt crash on a weird binary
# Add filter method for AcEvs?

# Should grouping default to both events and detection group? Or check other kind if
# first one has none (currently just stops if theres nothing there, this may be bad)

# TKEO Problems? See Sound Analysis With R book p427

# Its possible PamBinaries error handling needs a slight overhaul to be more R-like
# and less 'print' so they make more sense in other functions

# WHISTLES DAKINE
# Hop = (StartSample + 1) / (1st Slice Num)
# FFT Length = Samp Dur - (N slices - 1) * Hop(above)
# FFT Length = SampleRate * max(peakData) / MaxFreq
# Cepstrum : BinNum / SR = ICI   (This isn't dependent on FFT size bc we invert at same kine)

# BANTER EXPORT
# Need standard "species 100% id for training". Somehwere in specClass (this is a shitty name)
# Should this dump NAs and warn?

# IMPORTANT
# If you make changes to a function, need to add it again

# MISC PACKAGE
# Doc Van Cise / Dave Mellinger: SNR estimator in Matlab

# Run Banter - need to spit out info for Tethys and save the training set as an
# Rdata somewhere. Somehow named smartly and we save the name of banter run and
# the location it is stored on our server

# Should export_banter dump NAs and warn?

## PAMGUIDE NATHAN MERCHANT
# Noise stuff measurement in R ??

# TODO:
# Calibration - optionally use channel as argument?
# Enviro
# Add note to readme for adding functions: use PKG::FUN for th ings
# Double check/re-think pamguard dependencies for the future
# Mean spectra for an event
# Shorten wigner window back to same amount
# SHOW FUNCTIONS - X-Axes all same units in time, Y-axes same units

# What if no matching UIDs are found - AutoPAM test case binaries just arent the right ones.
# Need some kind of check if data is empty what to do

# Error log instead of individual warnings... ugh

# No filter option (click calcs). This made weirdness. Better to set filter at like 1Hz? or find
# a good normalizing option? unclear, need research

# Should PRS always be paired with output?

# Calibration - can we just check sign to decide add or subtract? Add options if we need to

# TUTORIAL: Examples of investigating specific detections with getBinaryData, showSTUFF functions

# DILEMMA: I can make things work probably a little sexier by making more things into S4/3 classes
# (having plotting function for calibration function maybe?), but then it gets weirder for some
# rando to use. Balancing that kind of weirdness (WTF is a CalSettingsDOnger object?) is hard. I
# dont know what is less confusing - more stuff done with less work with weird names I dont
# know how to interact with?

# CALIBRATION - calibration must be set to NULL, check if not NULL then we search globalenv for PRS
# objects, then search those for calibration with matching name. Spits out df with 'Frequency' and
# 'dB' in 20log10(spec) units dB

# CALIBRATION REDO - can I use get/set in a closure to make the calibration shit work?

# TUTORIAL / README / DOCUMENTATION - should have a list of warnings with more explanations

# NEW POSSIBILITY can we have a PAMr function class that can contain a calibration? maybeeeeee

# on.exit(add=TRUE) is a thing that always runs when function goes regardless of how it ends

# for now have suppressed warnings if event grouping is multiple, may want some kine warning

# ACEV update show method to be more relevant

# What if we only want basic info, no functions? At least keep time too. ie. read all binaries
# option. Thought came from talking to rando-Chris

# NOTES FROM PSAW:
# MAPPING: leaflet package for interactive maps seems preettttyyyy sweet
# WignerCalcs - use `sf` package can centroid ez bro
# gganimate seems fun. If theres a use case?
# Notes
# ggspatial package allows scale bars and shit
# geom_sf is now in ggplot2, native support for sf spatial objects,
# uses coord_sf instead of coord_map
# ANOTHAONE:
# what does purrr::walk do? used it to unzip some shit
