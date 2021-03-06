## The PAMrSettings object

The first step in using PAMr is to create a PAMrSettings object. This is an
S4 class created for this package, and will store all settings related to a
particular anaylsis. The goal of this object is to make it easy to share and
replicate results between users. All you need to do is send someone your
PAMrSettings object and they can see exactly how you analysed your data, or 
even point the PRS at their own data and process it in the exact same way.

A PAMrSettings object has four slots:

* db - This stores the full path to any SQLite databases that will be analysed
* binaries - This stores the folder names of any binaries, as well as the full
path to all individual Pamguard binary (.pgdf) files within those folders
* functions - This stores a set of functions that will be applied to the data 
when read in from the database/binary files. Stores functions for the 
'ClickDetector', 'WhistlesMoans', and 'Cepstrum' modules separately. When adding a new 
function, users are asked to supply values for any arguments of the function.
These values are saved with the function and cannot be changed without removing
the function entirely. 
* calibration - Stores a calibration function to correct for different hydrophone
characteristics. Optional, currently only affects certain 'ClickDetector' functions

Note that none of these slots should ever be manually edited, use the functions described
below if you want to add or remove anything.

A PAMrSettings object can be created without supplying any arguments (as described in the
[quick start guide](README.md)), or it can be created by directly supplying the database and binary
file paths, although the user will still be asked to input parameters for the 
`standardClickCalcs` function.

```r
myDb <- './Data/TestDB.sqlite3'
myBinaryFolder <- './Data/Binaries'
myPrs <- PAMrSettings(db = myDb, binaries = myBinaryFolder)
```
#### Adding to Your PRS

After the initial set-up of your PRS, you may want to add to it. There are 
four functions that accomplish this: `addDatabase`, `addBinaries`,
`addFunction`, and `addCalibration`. The first two are simple, and can be called interactively
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

### Adding from another PRS


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
