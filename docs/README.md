# PAMr

This is a package for processing passive acoustic data. Currently only supports
data collected using [Pamguard][pamguard], but in the future
we hope to support other platforms. 

Note that this is currently a beta version, so some bugs and quirks are expected.
Hopefully this guide will work you through the most common pitfalls, but please
report any other issues to [taiki.sakai@noaa.gov](mailto:taiki.sakai@noaa.gov).

Our goal is to make your life easier, so if you have any other suggestions or 
feedback feel free to pass that along as well!

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

#### Common Installation Issues

* If you are getting errors related to "Unable to unload package ______", try
opening a fresh R session, or installing the package from the R console instead
of RStudio

* If you see an error like `Error in strptime(xx, f, tz = tz) : unable to identify current timezone 'C'`,
then run the code `Sys.setenv(TZ = 'UTC')` and try again. This is common on field laptops, or laptops
where the timezone has been set manually to something other than the local timezone. 

* If you see an error like 
`Error: Failed to install 'PAMr' from GitHub: installationg of package 'ProblemPackage' had non-zero exit status`,
try installing the package "ProblemPackage" separately. PAMr relies on quite a few other packages, if any of them
fail to install properly then PAMr will not install. Sometimes a package will request to be "Installed from
source", try both options if it fails to install.

---

### Quick Start Guide

PAMr is currently only built to work with [Pamguard][pamguard],
and is built to organize your acoustic detections into events. Before getting
started you'll need to have three things:

1. The database (or multiple) created by Pamguard
2. The folder containing containing the binary files (xxx.pgdf) created by Pamguard
3. A way of organizing your data into events, either using Pamguard's event or 
Detection Group Localizer modules, or by specifying start and end times (see
[guide][time-grouping] for details on how to do this)

#### Create a PAMrSettings Object

Once you have these ready, the first step to using PAMr is to create a
PAMrSettings object (PRS) using the `PAMrSettings()` function. You can call this 
function with no arguments and pop-up menus will walk you through the rest:

```r
myPrs <- PAMrSettings()
```
First you will be asked to select the database files. You can select more than one
using CTRL or SHIFT. Don't worry - you can add or remove databases later, nothing
here is final.

![*Selecting database files*][db-select-image]

Next you will be asked to select a folder containing the binary files. You can
just select the highest level folder and PAMr will search through all the 
sub-folders for any .pgdf files (ie. it is okay to select "Binaries" instead of
"20040828" in the image below).

![*Selecting the binary folder*][binary-select-image]

**NOTE:** Sometimes the selection windows don't pop up in front of your R session,
if you don't immediately see the file selection window for either of these two
steps then look for it with ALT+TAB.

Next you will be asked to set some parameters for the processing functions that
come with PAMr. There is one function for each of three types of detections -
clicks, whistles, and cepstrum detections. See [here][standard-calcs] for more
information.

The only function you need to set parameters for is the "standardClickCalcs"
function. You can either enter a new value for the parameter it is asking you
to set, or just pressing ENTER will use the default value (the default value is
shown. There are three parameters you will be asked to set 
(more details found [here][standard-calcs]):

1. "sr_hz" - the sample rate in hertz for the click data. This should be kept 
to the default value of "auto" (just press ENTER) unless your click data was
decimated, in which case enter the decimated sample rate.
2. "highpass_khz" - the value in kilohertz of the butterworth highpass filter
to apply to the click data. If 0, then no filter will be applied.
3. "winLen_sec" - the length of the FFT window to use for analysis, in seconds

![*Setting a value of 5 for the highpass filter*][function-param-image]

**NOTE:** You will need to enter these values in the R Console, so if you are running
`PAMrSettings()` from a script you need to either click in the console before typing
or use the shortcut CTRL+2

Once you've finished entering values for these (or accepting the defaults), you're 
ready to process some data! For more details about the PRS, including adding or 
removing data or functions, see the [PAMrSettings page][pamrsettings]

#### Processing Your Data

Once you have a PRS, processing your data is easily accomplished by calling
the `processPgDetections` function. This function has two modes, `mode = 'db'`
for processing events labeled within Pamguard, and `mode = 'time'` for events
labelled by start and end times. For "db", you can just run:

```r
myStudy <- processPgDetections(myPrs, mode='db', id='MyStudyName')
```

Note that if `id` is not specified it will default to today's date, but it is
always recommended to provide an informative name.

For "time", you will also need to supply your event data, see [this guide][time-grouping]
for details on how this should be formatted:

```r
myStudy <- processPgDetections(myPrs, mode='time', id='MyStudyName', grouping='myEvents.csv')
```

**NOTE:** When running `mode='time'`, you may be prompted to make some decisions about matching
databases and events, or have issues converting dates from a csv file. See [here][time-grouping]
for more details about what's going on.

Describe what your data output is - Study/Event
Then more details about your PRS - adding, customizing, what it has

PAMr works by grouping acoustic detections into events. Either in Pamguard or
by start/end times. 

### Next Steps

To learn more about what PAMr can do for you, click on the links below

PRS details - adding and removing stuff
Adding GPS data
Assign species ID
Adding calibration function for clicks
Creating custom functions
Accessing your data - $, getDetectorData
Study/Event details
Collaboration
Common error messages ?
Currently in development
Future plans

### Versions

For bug fixes and feature additions in each version see the [NEWS](../NEWS.md) file

[standard-calcs]: StandardCalcs.md
[time-grouping]: TimeGrouping.md
[db-select-image]: images/DBSelectCropped.png
[binary-select-image]: images/BinarySelectCropped.png
[function-param-image]: images/FunctionParamsCropped.png
[pamguard]: https://www.pamguard.org/
[pamrsettings]: PAMrSettings.md

