## PAMr 0.7.0

### Functions Renamed

* `getPgDetections` has now been re-named to `processPgDetections` to more accurately
reflect what it is doing. Functions starting with `get___` will be used just for 
accessing data

* `showWaveform` and related functions have been renamed to `plotWaveform`

* General naming consistency overhaul - `sr` and `db` should now be used in place of
`sampleRate` and `database` wherever these were previously used, and this should be
the naming convention going forward. This means included functions will need to be 
re-added (they previously relied on it being named `sampleRate`), and any custom 
functions should expect data to contain the item `sr` and not `sampleRate`

### Major Changes

* `AcousticStudy` class has been added. This will now be the class of object
returned by `processPgDetections`, it stores your list of `AcousticEvent` objects with
other important data. 

* Any data processed in previous versions will need to be re-run, and all functions in
older `PAMrSettings` files will need to be removed and re-added 

* New Function: `calculateICI` calculates the inter-click interval of click data, and 
stores it in the `ancillary` slot of an `AcousticEvent`. 

* New Function: `plotDataExplorer` creates an interactive plot to allow users to 
explore the data in an `AcousticStudy`, `AcousticEvent`, or `data.frame`. Plot
can be colored or facetted by any columns that are characters or factors, and any
numeric columns can be plotted with `ggplot`'s `geom_density`

* New Function: `getDetectorData` gathers all detector data into single data frames
for each detector type (`'click'`, `'whistle'`, and `'cepstrum'`) for easier manipulation

### Minor Changes

* Detector dataframes in `AcousticEvents` now have a `'calltype'` associated with them,
currently one of '`whistle'`, `'click'`, or `'cepstrum'`. This allows for future fun stuff
to happen

* `getBinaryData` will now attempt to get the appropriate sample rate for each
data point, either from the settings or matching by time using the database file
if more than one sample rate was in your data.

* Many speed improvements - click calculations should take about half the time, and
`processPgDetections` with `mode='time'` will now skip over binaries that are outside
of the time range of specified events

* `getPgDetections` no longer has an option `mode='all'`, and it is no longer necessary
to supply a sample rate for `mode='time'`. It will now attempt to match events to a
correct database used on time stamps, the match the appropriate sample rate the same
way that `mode='db'` does. If a manual sample rate needs to be applied it should now be
entered into the `grouping` table

* Functions `addBinaries` and `addDatabase` can now add files from another `PAMrSettings`
object, and will report how many files have been added when finishing

* `export_banter` will now attempt to find event level measures in the `'measures'` item in
the `ancillary` slot of each `AcousticEvent`. 

* `export_banter` no longer has a `reportNA` option, any `NA` values that are removed are now
always saved to a separate `'na'` item in the list output. The function also reports how many
detections were processed.

* `addGps` now also stores all of the gps data loaded into the `gps` slot of the `AcousticStudy`
instead of only adding coordinates to detections

## PAMr 0.6.8

* `getPgDetections` will name events with database appended for `method = 'db'` instead
of just event ID number to ensure uniqueness across multiple databases

## PAMr 0.6.7

* I don't remember what happened here. I bet it was important

## PAMr 0.6.6

* `setSpecies` can now use a dataframe for `method = 'manual'`, and has
a top secret option for SR

## PAMr 0.6.5

* Updated `export_banter` with options to exclude certain species and to
export data without species codes to use for prediction only instead
of training a banter model

## PAMr 0.6.4

* Better error tracking when functions cause `getPgDetections` to crash

* Now reads in angles and angleErrors from click data

* `export_banter` allows you to specify certain columns to not export

* `'time'` mode for `getPgDetections` will now report a sample event time
so you can see if times are being converted properly from your csv before
proceeding with calculations

## PAMr 0.6.3

* Fixed an issue where `getPgDetections` would not work if both Detection Group Localizer
and Offline Click events were present in a database.

* Renamed `eventType` and `Text_Annotation` columns from event databases to `eventLabel`
within detection dataframes so there is consistency between the two

* Removed some unnecessary columns from detection dataframes, including `detectorName`,
`sampleRate`, `Id`, `parentUID`, and `comment`

* Fixed a bug in how `getBinaryData` was checking for multiple matches on the same UID

## PAMr 0.6.2

* Fixed an issue with repeated entries in click detections for modes other than `'db'`

## PAMr 0.6.1

* Sometimes whistles would not get proper decimated sample rate, fixed

## PAMr 0.6.0

* Added an `id` slot to `AcousticEvent` objects. Note that this will cause existing `AcousticEvent`
objects to behave poorly until they have their `id` slot created / updated using the new
`setIdSlot` function.

* Added `setIdSlot` function to update older `AcousticEvent` objects to the new format

## PAMr 0.5.9

* Fixed bug in SR / FFT parameter calculation in whistles if there was a gap in
the whistle contour

## PAMr 0.5.8

* Changed event naming for `mode='time'` in `getPgDetections`. Will now only append
numbers if event is not unique, and will also insert an underscore before the number

* `export_banter` now properly checks for cases when there are no detectors in an event
instead of crashing confusingly

## PAMr 0.5.7

* Rocca whistle calcs minor change - boundary settings for sweeps set to "flat",
should reduce number of inflection points

## PAMr 0.5.6

* minor change to `export_banter` no longer using list names to index and create unique event names

## PAMr 0.5.5

* `export_banter` now removes NA rows, and has reportNA option to see which ones are NA

* Detector names have spaces replaced with _ to avoid weird issues later

## PAMr 0.5.4

* Dealing with zero detection events better for `export_banter`

* Temporary fix for click calculations with lots of zeroes in wave form - no more NA

## PAMr 0.5.3

* `standardClickCalcs` now supports manual input of sample rate. Default argument is `'auto'`,
which will read from the database. User can supply a numeric value for sample rate in hertz instead.

## PAMr 0.5.2

* Added a check in `addDatabase` to see if all files are actually .sqlite3 databases

* Added a `tryCatch` in `getPgDetections` for mode `db` so that it shouldn't 
stop completely when encountering an error and lose all previously analysed DBs

## PAMr 0.5.1

* Minor bug fix when using `seewave::spec`, it can produce NA values for the frequency
if the input wave is long. Adjusted parts `standardClickCalcs` and `addCalibration` to
work around this.

## PAMr 0.5.0

* `getPgDetections` changed to work by specifying a `mode` as an argument instead of
calling separate functions. Can now create events using a csv or dataframe with 
start and end times specified.

* Changed `export_banter` to export a list with named item `detectors` instead of 
`detections`, no other functional change.

## PAMr 0.4.0

* Added `setSpecies` functions for assigning species classifications to 
AcousticEvents.

* Changed `getDbData` to default to looking for both OfflineEvents tables 
and DetectionGroupLocaliser tables, will still only load one if a specific
type is provided for `grouping`.

## PAMr 0.3.0

* Added `addCalibration`, `applyCalibration`, and `findCalibration` functions,
as well as a `plot` method that will show the calibration function used. See
the  *Calibration* section above for more details.

* Fixed a bug in `removeFunction` that would cause the incorrect number of
functions to show in certain cases, and that would cause all functions to
be removed when only one was selected.

* `standardClickCalcs` has been adjusted to work with the new calibration
methods. See *Calibration* section above for more details.

## PAMr 0.2.2

* Added `addGps` function for adding matching GPS data to your detections. This 
allows you to supply a dataframe of Lat/Long locations with timestamps to 
match to your detections.

* Added `showWaveform`, `showSpectrogram`, and `showWigner` functions that
allow you to easily plot the waveform, spectrogram, or wigner plot of a detection
in an `AcousticEvent` object by selecting the UID(s) you want to investigate further.
`getBinaryData` is also added as a helper function for these, lets you easily
get the binary file data for a single detection.

* `standardClickCalcs` now supports supplying a `Wave` class object as input

## PAMr 0.2.0

* Rocca (`roccaWhistleCalcs`) and cepstrum (`standardCepstrumCalcs`) 
functions added. These are also added by default to a new PRS.

* changed `AcousticEvent` class slot name from `specClass` to `species`
