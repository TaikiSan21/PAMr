source('./devel/PAMr_functions_anne.R')
# source(file.choose())
library(PAMr)
hp30 <- function(x) PAMr::standardClickCalcs(x, highpass=30)
myFuns <- list('ClickDetector' = list(PAMr::standardClickCalcs, hp30))
myFuns <- list('ClickDetector' = list(PAMr:: standardClickCalcs))
# Change these paths to location of db / binary folder
myDb <- '../Data/ArrayTest/Database/TestDb.sqlite3'
binaryFolder <- '../Data/ArrayTest/Binaries'
myList <- list.files(binaryFolder, recursive=TRUE, full.names=TRUE, pattern='pgdf')

# Loads all your data & does calculations. Can take a while if you have lots.
myData <- getDetectionsFromDb(myDb, myList, myFuns)
myData <- getAllDetections(myList[1:5], myFuns)

myData <- getPgDetectionsAll(myList, myFuns, sampleRate = 48e3)
myData <- getPgDetections(db=myDb, binFuns=myFuns, binList=myList)
profvis::profvis(getPgDetections(db = myDb, binList = myList,binFuns = myFuns))
# 10850, 6510 loadpg. 8700, 6700.
# These two format your data into the 'detections' and 'events' style that banter wants.
banterDetections <- banter_exportDetections(myData)
banterEvents <- banter_exportEvents(myData)

# PRS STYLE
myPrs <- PAMrSettings(db = myDb, binaries = binaryFolder)
myDataPrs <- getPgDetections(myPrs)
myData <- getPgDetectionsAll(myPrs, sampleRate=48e3)
profvis::profvis(getPgDetections(myPrs))
# SR Test
myDb <- 'C:/AutoPAM/AutoPAM_Test_2018.sqlite3'
binFolder <- 'C:/AutoPAM/Binaries'
srPrs <- PAMrSettings(db = myDb, binaries = binFolder)
srData <- getPgDetections(srPrs)
