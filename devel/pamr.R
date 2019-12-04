

# Offline event id in rocca csv -> eventid in offline clicks table. CSV might be same as rocca whistle stats table? Duration is 0 for clicks tho

## DETCTOR NAMES
# For seeing what detectors we have - PamguardModules table will have a thing for each detector (seems every time its run, or finds something).
# Module_Name has WhistlesMoans and Click Detector, then Module_Type has the name of the detector. These correspond to table names.
# For clicks: Name_of_detector_(Background/Clicks/OfflineClicks/OfflineEvents)
# For WM: Name_of_detector and Name_of_detector_Localised_Name_of_detector_Contours
## BINARY names
# For clicks: Click_Detector_Name_of_detector_(Clicks/Trigger_Background)_YYYYMMDD_HHMMSS.pgdf
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
# Add filter method for AcEvs?

# TKEO Problems? See Sound Analysis With R book p427

# Its possible PamBinaries error handling needs a slight overhaul to be more R-like
# and less 'print' so they make more sense in other functions

# WHISTLES DAKINE
# Hop = (StartSample + 1) / (1st Slice Num)
# FFT Length = Samp Dur - (N slices - 1) * Hop(above)
# FFT Length = SampleRate * max(peakData) / MaxFreq
# Cepstrum : BinNum / SR = ICI   (This isn't dependent on FFT size bc we invert at same kine)

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
# ***Mean spectra for an event***
# SHOW FUNCTIONS - X-Axes all same units in time, Y-axes same units

# What if no matching UIDs are found - AutoPAM test case binaries just arent the right ones.
# Need some kind of check if data is empty what to do

# Error log instead of individual warnings... ugh

# No filter option (click calcs). This made weirdness. Better to set filter at like 1Hz? or find
# a good normalizing option? unclear, need research


# Calibration - can we just check sign to decide add or subtract? Add options if we need to

# TUTORIAL: Examples of investigating specific detections with getBinaryData, showSTUFF functions

# DILEMMA: I can make things work probably a little sexier by making more things into S4/3 classes
# (having plotting function for calibration function maybe?), but then it gets weirder for some
# rando to use. Balancing that kind of weirdness (WTF is a CalSettingsDOnger object?) is hard. I
# dont know what is less confusing - more stuff done with less work with weird names I dont
# know how to interact with?


# CALIBRATION REDO - can I use get/set in a closure to make the calibration shit work?

# TUTORIAL / README / DOCUMENTATION - should have a list of warnings with more explanations

# NEW POSSIBILITY can we have a PAMr function class that can contain a calibration? maybeeeeee

# for now have suppressed warnings if event grouping is multiple, may want some kine warning

# ACEV update show method to be more relevant

# "CalibrationUsed" in getPgDetections stuff

# picking out shit like EKG false detections needs to happen during analysis

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


# List only 20DBs at a time in removeDatabase(), add something to deal with that

# Different samplerates for different detectors - might decimate your whistle detector or some shit

# Tethys kine stuff -
# make a pop-up form prompt style thing for people to fill in whatever bits we are missing that we
# cant get from pamguard
# passivePacker program for NCEI - program runs in backend using sqlite, the fields in that form
# of what we need to populate usding PAMr. Make an sqlite db from PAMr to talk to tethys and all
# talk to eachother

# 'Cannot find event tables for grouping method "" ' error in pascal? why not named

# Add guideline for making new functions - new functions should check if user has all
# packages used there. Best practice for adding this to top of a function, update readme
# accordingly.

# pamr for individual calls? for whales
# probably take everything from detectors, not broken by events

# access using environment instead of list is fast??

# PAMGUARD BUG - in kogia JK sent me had min 90k max 20k in binary. wtf

# can menu force look at console? annoying to type things in not usr friendly
# KeyboardSimulator package could do ctr + 2 or also for mac check OS. maybe doesnt
# work

# TKE threshold on really clean clicks probably isnt doing what we want - like
# clicks with lots of zero's (JK e-mails circa 6/6 kogia). Insane low numbers for
# noise level if this is supposed to be similar to dB

# combine / add multiple AcEv

# hydrophone depth goes in somewhere

# currently have label in getDbData to choose between either eventType or comment, other is dropped
# not 100% sure about this, but otherwise inconsistency between two types of events. Can only allow
# so much wiggle room...

# should i change getBInaryData to work on file list? wont be huge for single event, can drop
# BinaryFile from each detection row... MAYBE CANT DO THIS YET noticing with multiple WM
# detector have UIDs starting at same place, so need to know which one to check

# Add test whistle and test cepstrum for people and function testing

# Add block in new peoples functions for loading / checking packages

# Suggest people run on base PAMr before adding new functions

# Figure out what to do when encountering garbo data that crashes shit in general

# For Yvonne - we probably just want to import localizations into PAMr, then have a
# GO DO DISTANCE part that will get detection functions and shit. And whatevre else.
# Should be easy? Talk to dave miller about getting detection functions from it

# Yvonne timeline - localization stuff done maybe mid-Nov or dec. By February have
# modeling set up with envir variables, ready to start trying out models.

# Plotting stuff - check number of points before plotting, obvs cant and dont want
# to do 100k points. So can ask if you want to plot all, or subset. Can have event
# species, or coords to subset by as options.

# read your banter predictions and results back in

# summaries could have different outputs based on different kinds of species classifications

# effort in study is probably a named list of DFs with start and stop times

# PAMr version info??? in Study

# PassivePacker / TETHYS stuff - deployment and recovery questions

# Label detectors with actual labels?

# event level measures????

# In future export_banter could take multiple studies?? maybe thats useful

# import_xx should ask for results and the model to store

# possibly rethink addGps for AcousticStudy and in general. Are we just adding to
# AS and storing? Match when needed?

# maybe show wigner should warn people if trying to plot a lot at once, shit is slow
# and sad

# ADD IN TUTORIAL add a db to prs even if you arent using getPgDetectionsDb

# Sam does DAS stuff we should give him more work to do
# - sighting locations and re-sights: we wanna make a map with acoustic
#       locations and vis locations both
# - we eventually want some kind of tool to help people decide if an acoustic and
#       visual thing are a match - time and location of sights, subgroups??
# - need basic access to all data like weather and effort
# - on the topic of DAS files - Jennifer has to do some hardcore matching nonsense,
#       so ask her what her "dream scenario" is for dealing with shit like FKW

# Can we read in just specific columns of tables? Confusing error from
# Shannons data about beam time column
`

### BATCH PAMRSETTINGS FILE
# Where we can specify clumps of files that belong together, but use the same functions
# for evreything else.

# Check annamaria data from shannon's drive to see what is causing the binary mismatch
# error. Should be able to find things with just ALL / ALL, why not??

# timezone missing error in pambinaries - FIX OR AT LEAST PUT IN TUTORIAL

# id clash from dplyr / tidyr or something

# on tutorial : using someone else's PRS file how-to

# PG binary not saving exactly raw audio wav of click - what gets done here?
# is okay to treat as raw for calibration purposes? PUT THIS IN DOCUMENTATION
# "we are treating this as raw audio wav"

# reach out to me or shannons poster, maybe I can see "i'll be at this poster tomorrow
# during session wahtever"

# highlight ease of use, flexibility with plugin functions, minimal lines of code,
# reproducibility examples

# currently available on github, up on CRAN next year, when it goes up we will post to
# marmam listserv
# sound_acquisiton table Start/Stop not always present for every wav file

# PAMr warnings sink function??? figure out categories

# ask Ravi about using Manta computer abroad

# could i make loadPamguardBinary a lot faster using .subset2 ???

# maybe can make GPS stuff faster with more extensive datatable fuckjing

# should cepstrumb e bp?

# for mapping function can check lat long exists, if it doesnt if you do
# addGps on list of AcEv not study it wont store just match

# shiny app not needed to host on a website, can run locally. maybe a good replacement
# for dataexplorer plot and other future plots
