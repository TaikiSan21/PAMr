# PAMr

This is a very beta version of a package for processing passive acoustic data.
User beware!

### Installation

Install the latest version from GitHub:

```r
# make sure you have Rtools installed
if(!require('devtools')) install.packages('devtools')
# install from GitHub
devtools::install_github('TaikiSan21/PAMr')
```

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

Once you have a PRS object, processing your data is easy. There are two
functions that do this, `getPgDetectionsDb` and `getPgDetectionsAll`. The "Db"
function just requires the PRS as input, and will load all of the data found
in the `db` slot of your PRS object. It will group these detections into events
based on either the Detection Group Localiser module, or the Click Event module.
All functions in the `functions` slot of the PRS will be applied to the appropriate
module type, and the results will get stored as a dataframe. The "All" function
requires the PRS and the sample rate of your data as inputs (normally this is read
from a database), then applies all the calculations.

```r
myDataDb <- getPgDetectionsDb(myPrs)
myDataAll <- getPgDetectionsAll(myPrs, 192000)
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

WhistlesMoans function are not yet implemented.

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

