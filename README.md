# PAMr

This is a very beta version of a package for processing passive acoustic data.
User beware!

### Installation

To install the latest version from GitHub, first make sure that you have
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed, then 
run the following code to install PAMr and two supporting packages, PamBinaries
and PAMmisc. Currently it is important to install PamBinaries and PAMmisc first,
but this won't matter once the package is on CRAN (Est. Q1 2020).

```r
# make sure you have Rtools installed
if(!require('devtools')) install.packages('devtools')
# you will also need these packages from GitHub
devtools::install_github('TaikiSan21/PamBinaries')
devtools::install_github('TaikiSan21/PAMmisc')
# install from GitHub
devtools::install_github('TaikiSan21/PAMr')
```

##### Common Installation Issues

* If you are getting errors related to "Unable to unload package ______", try
opening a fresh R session, or installing the package from the R console instead
of RStudio

* If you see an error like `Error in strptime(xx, f, tz = tz) : unable to identify current timezone 'C'`,
then run the code `Sys.setenv(TZ = 'UTC')` and try again. This is common on field laptops, or laptops
where the timezone has been set manually to something other than the local timezone. 

* If you see an error like 
`Error: Failed to install 'PAMr' from GitHub: installationg of package 'PACKAGENAME' had non-zero exit status`,
try installing the package `PACKAGENAME` separately. PAMr relies on quite a few other packages, if any of them
fail to install properly then PAMr will not install.

### Getting Started


### Tutorial

The first step in using PAMr is to create a PAMrSettings object. This is an
S4 class created for this package, and will store all settings related to a
particular anaylsis. The goal of this object is to make it easy to share and
replicate results between users. 

A PAMrSettings object has four slots:

* db - This stores the full path to any SQLite databases that will be analysed
* binaries - This stores the folder names of any binaries, as well as the full
path to all individual Pamguard binary (.pgdf) files within those folders
* functions - This stores a set of functions that will be applied to the data 
when read in from the database/binary files. Stores functions for the 
'ClickDetector' and 'WhistlesMoans' modules separately. When adding a new 
function, users are asked to supply values for any arguments of the function.
These values are saved with the function and cannot be changed without removing
the function entirely. 
* calibration - Currently does nothing, in later versions will store calibration
functions to apply to data before making any calculations.

#### Getting Started 

A new PAMrSettings (PRS) object can be created interactively by simply calling the
function `PAMrSettings()`:

```r
myPrs <- PAMrSettings()
```

*** Note that only Click and WhistlesMoans binaries will be added ***

This will ask the user to select a database file, then select a folder of binaries,
and then ask for parameters for the included `standardClickCalcs` function. In
future versions there may be more "standard" functions included. If you do not
have a database or binary folder, hitting "Cancel" on the selection window will
still create the PAMrSettings object, just without those files.

A PAMrSettings object can also be created by directly supplying the database and binary
file paths, although the user will still be asked to input parameters for the 
`standardClickCalcs` function.

```r
myDb <- './Data/TestDB.sqlite3'
myBinaryFolder <- './Data/Binaries'
myPrs <- PAMrSettings(db = myDb, binaries = myBinaryFolder)
```

#### Processing Data

Once you have a PRS object, processing your data is easy. There are three ways
to do this, specified using the `mode` argument of `getPgDetections`. Mode `db`
just requires the PRS as input, and will load all of the data found
in the `db` slot of your PRS object. It will group these detections into events
based on either the Detection Group Localiser module, or the Click Event module.
All functions in the `functions` slot of the PRS will be applied to the appropriate
module type, and the results will get stored as a dataframe. Mode `all`
requires the PRS and the sample rate of your data as inputs (normally this is read
from a database), then applies all the calculations to every detection in every binary
file. Mode `time` requires a dataframe or csv file specifying the start and end times
of events as well as an event id and optionally a species id. See `?getPgDetections` for
more info on this mode.

```r
myDataDb <- getPgDetections(myPrs, mode='db')
myDataAll <- getPgDetections(myPrs, mode='all', sampleRate=192000)
myDataTimes <- getPgDetections(myPrs, mode='time', sampleRate=192e3, grouping='myEvents.csv')
```

#### Adding to Your PRS

After the initial set-up of your PRS, you may want to add to it. There are 
three functions that accomplish this: `addDatabase`, `addBinaries`, and
`addFunction`. The first two are simple, and can be called interactively
just like the initial PRS setup or by providing the paths.

```r
myPrs <- addDatabase(myPrs)
myPrs <- addBinaries(myPrs)
newDb <- './Data/NewDB.sqlite3'
newBinaries <- './Data/NewBinaries/'
myPrs <- addDatabase(myPrs, newDb)
myPrs <- addBinaries(myPrs, newBinaries)
```

Adding a function is slightly more involved. First make sure the function (or
the package the function is in) is already sourced. Then add the function by
name, also specifying the module as either 'ClickDetector' or 'WhistlesMoans'.
If you do not specify, you will be asked to choose. `addFunction` will also
ask the use to set the value for any parameters that are arguments to the
function you provide, except for parameters named "data" or "calibration".
In this example, the user would be ask to set values for "a", then "b", and
would be told that the default for "a" is 1, and that there is not a default
value for "b".

```r
testFunction <- function(data, a=1, b) {
    ### Do smart stuff here
}
myPrs <- addFunction(myPrs, testFunction, module='ClickDetector')    
```

For the ClickDetector, there are a couple of requirements for this function. 
The function should have an input called "data", and it should expect that 
this "data" input is a list with two parts: `data$wave` containing the click 
waveform, and `data$sampleRate` containing the sample rate of this click. The
waveform will have one column for each channel. The output of this function
should be a dataframe that has one row for each channel, there can be as many 
columns as you like. `addFunction` will do a quick check of the function you
try to add on a sample click.

For WhistlesMoans functions, the function should have an input "data" that is
a list with two parts: `data$freq` and `data$time`. `freq` should contain the
contour of the whistle, stored as a vector of frequencies. `time` should be
the time in seconds at each of these frequencies. The output of this function
should be a dataframe with one row, or an object that is easily coerced to a
dataframe (e.g. a list where all elements have length 1). 

Functions can also be added from a separate PRS object by passing the other
PRS object in as the second input. In this case all functions from the second
PRS object (`newPrs` below) will be added to your PRS. This can be useful when
collaborating with others. Someone can send you their PRS file with their functions
and settings, but the paths to the data will not match up on your computer. You
can use this method to add their calculations to your PRS file that points to your
data.

```r
newPrs <- myPrs
myPrs <- addFunction(myPrs, newPrs)
```

#### Calibration

*under construction*

!!IMPORTANT!! If you are adding your own functions to a PRS object and you make
a change to your function and source it again, the function *will not be changed*
in the existing PRS object. You must remove the function and add it again, bringing
us to the next section...

#### Removing Things From Your PRS

There are three functions that remove items from your PRS, and it is recommended
that you use this instead of trying to alter the PRS manually. All of them can
be called interactively and provide menus for the user to select the item to 
remove. See the help pages for more info.

```r
myPrs <- removeDatabase(myPrs)
myPrs <- removeBinaries(myPrs)
myPrs <- removeFunction(myPrs)
```

### Versions

<<<<<<< HEAD
**0.7.0**

* Detector dataframes in `AcousticEvents` now have a `'calltype'` associated with them,
currently one of `whistle`, `click`, or `cepstrum`. This allows for future fun stuff
to happen

* `getPgDetections` has now been re-named to `processPgDetections` to more accurately
reflect what it is doing. Functions starting with `get___` will be used just for 
accessing data

* Major update adding in `AcousticStudy` class. This will now be the class of object
returned by `getPgDetections`, it stores your list of `AcousticEvent` objects with
other important data. 

* `getBinaryData` will now attempt to get the appropriate sample rate for each
data point, either from the settings or matching by time using the database file
if more than one sample rate was in your data.

* Many speed improvements - click calculations should take about half the time, and
`getPgDetections` with `mode='time'` will now skip over binaries that are outside
of the time range of specified events

* General naming consistency overhaul - `sr` and `db` should now be used in place of
`sampleRate` and `database` wherever these were previously used, and this should be
the naming convention going forward. This means included functions will need to be 
re-added (they previously relied on it being named `sampleRate`)

* `getPgDetections` no longer has an option `mode='all'`, and it is no longer necessary
to supply a sample rate for `mode='time'`. It will now attempt to match events to a
correct database used on time stamps, the match the appropriate sample rate the same
way that `mode='db'` does. If a manual sample rate needs to be applied it should now be
entered into the `grouping` table

* `showWaveform` and related functions have been renamed to `plotWaveform`

**0.6.10**

* temporary ICI calculations for banter export in `banterICI`, this will be removed in
v 0.7.0

* `standardClickCalcs` also adds in a `peakTime` value that is the time of the peak of 
the waveform within the click

**0.6.9**

* minor changes in `setSpecies`

**0.6.8**

* `getPgDetections` will name events with database appended for `method = 'db'` instead
of just event ID number to ensure uniqueness across multiple databases

**0.6.7**

* I don't remember what happened here. I bet it was important

**0.6.6**

* `setSpecies` can now use a dataframe for `method = 'manual'`, and has
a top secret option for SR

**0.6.5**

* Updated `export_banter` with options to exclude certain species and to
export data without species codes to use for prediction only instead
of training a banter model

**0.6.4**

* Better error tracking when functions cause `getPgDetections` to crash

* Now reads in angles and angleErrors from click data

* `export_banter` allows you to specify certain columns to not export

* `'time'` mode for `getPgDetections` will now report a sample event time
so you can see if times are being converted properly from your csv before
proceeding with calculations

**0.6.3**

* Fixed an issue where `getPgDetections` would not work if both Detection Group Localizer
and Offline Click events were present in a database.

* Renamed `eventType` and `Text_Annotation` columns from event databases to `eventLabel`
within detection dataframes so there is consistency between the two

* Removed some unnecessary columns from detection dataframes, including `detectorName`,
`sampleRate`, `Id`, `parentUID`, and `comment`

* Fixed a bug in how `getBinaryData` was checking for multiple matches on the same UID

**0.6.2**

* Fixed an issue with repeated entries in click detections for modes other than `'db'`

**0.6.1**

* Sometimes whistles would not get proper decimated sample rate, fixed

**0.6.0**

* Added an `id` slot to `AcousticEvent` objects. Note that this will cause existing `AcousticEvent`
objects to behave poorly until they have their `id` slot created / updated using the new
`setIdSlot` function.

* Added `setIdSlot` function to update older `AcousticEvent` objects to the new format

**0.5.9**

* Fixed bug in SR / FFT parameter calculation in whistles if there was a gap in
the whistle contour

**0.5.8**

* Changed event naming for `mode='time'` in `getPgDetections`. Will now only append
numbers if event is not unique, and will also insert an underscore before the number

* `export_banter` now properly checks for cases when there are no detectors in an event
instead of crashing confusingly

**0.5.7**

* Rocca whistle calcs minor change - boundary settings for sweeps set to "flat",
should reduce number of inflection points

**0.5.6**

* minor change to `export_banter` no longer using list names to index and create unique event names

**0.5.5**

* `export_banter` now removes NA rows, and has reportNA option to see which ones are NA

* Detector names have spaces replaced with _ to avoid weird issues later

**0.5.4**

* Dealing with zero detection events better for `export_banter`

* Temporary fix for click calculations with lots of zeroes in wave form - no more NA

**0.5.3**

* `standardClickCalcs` now supports manual input of sample rate. Default argument is `'auto'`,
which will read from the database. User can supply a numeric value for sample rate in hertz instead.

**0.5.2**

* Added a check in `addDatabase` to see if all files are actually .sqlite3 databases

* Added a `tryCatch` in `getPgDetections` for mode `db` so that it shouldn't 
stop completely when encountering an error and lose all previously analysed DBs

**0.5.1**

* Minor bug fix when using `seewave::spec`, it can produce NA values for the frequency
if the input wave is long. Adjusted parts `standardClickCalcs` and `addCalibration` to
work around this.

**0.5.0**

* `getPgDetections` changed to work by specifying a `mode` as an argument instead of
calling separate functions. Can now create events using a csv or dataframe with 
start and end times specified.

* Changed `export_banter` to export a list with named item `detectors` instead of 
`detections`, no other functional change.

**0.4.0**

* Added `setSpecies` functions for assigning species classifications to 
AcousticEvents.

* Changed `getDbData` to default to looking for both OfflineEvents tables 
and DetectionGroupLocaliser tables, will still only load one if a specific
type is provided for `grouping`.

**0.3.0**

* Added `addCalibration`, `applyCalibration`, and `findCalibration` functions,
as well as a `plot` method that will show the calibration function used. See
the  *Calibration* section above for more details.

* Fixed a bug in `removeFunction` that would cause the incorrect number of
functions to show in certain cases, and that would cause all functions to
be removed when only one was selected.

* `standardClickCalcs` has been adjusted to work with the new calibration
methods. See *Calibration* section above for more details.

**0.2.2** 

* Added `addGps` function for adding matching GPS data to your detections. This 
allows you to supply a dataframe of Lat/Long locations with timestamps to 
match to your detections.

* Added `showWaveform`, `showSpectrogram`, and `showWigner` functions that
allow you to easily plot the waveform, spectrogram, or wigner plot of a detection
in an `AcousticEvent` object by selecting the UID(s) you want to investigate further.
`getBinaryData` is also added as a helper function for these, lets you easily
get the binary file data for a single detection.

* `standardClickCalcs` now supports supplying a `Wave` class object as input

**0.2.0** 

* Rocca (`roccaWhistleCalcs`) and cepstrum (`standardCepstrumCalcs`) 
functions added. These are also added by default to a new PRS.

* changed `AcousticEvent` class slot name from `specClass` to `species`

=======
For bug fixes and feature additions in each version see the [NEWS](NEWS.md) file
>>>>>>> study
