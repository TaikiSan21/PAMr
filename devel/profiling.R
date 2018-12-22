library(microbenchmark)
library(profvis)
testClick <- '../Data/ArrayTest/TestBin/20170929/Click_Detector_Click_Detector_Clicks_20170929_080000.pgdf'
cl <- loadPamguardBinaryFile(testClick, skipLarge=F)
allUIDs <- as.numeric(names(cl$data))
minUID <- min(allUIDs)
maxUID <- max(allUIDs)
some <- sample(allUIDs, 10)
profvis({
  stuff <- getDetectionsFromDb(dbFile, binList)
})
profvis({
  try1 <- loadPamguardBinaryFile(testClick, keepUIDs=minUID)
  try2 <- loadPamguardBinaryFile(testClick, keepUIDs=maxUID)
  try3 <- loadPamguardBinaryFile(testClick, keepUIDs=minUID, skipLarge=T)
  try4 <- loadPamguardBinaryFile(testClick, keepUIDs=maxUID, skipLarge=T)
  try5 <- loadPamguardBinaryFile(testClick, keepUIDs=some, skipLarge=T)
  try6 <- loadPamguardBinaryFile(testClick, keepUIDs=some, skipLarge=F)
})

microbenchmark(
  try1 = loadPamguardBinaryFile(testClick, keepUIDs=minUID),
  try2 = loadPamguardBinaryFile(testClick, keepUIDs=maxUID),
  try3 = loadPamguardBinaryFile(testClick, keepUIDs=minUID, skipLarge=T),
  try4 = loadPamguardBinaryFile(testClick, keepUIDs=maxUID, skipLarge=T),
  try5 = loadPamguardBinaryFile(testClick, keepUIDs=some, skipLarge=T),
  try6 = loadPamguardBinaryFile(testClick, keepUIDs=some, skipLarge=F),
times=20)

microbenchmark(
  big = loadPamguardBinaryFile(testClick),
  small = loadPamguardBinaryFile(testClick, skipLarge = TRUE),
  date = loadPamguardBinaryFile(testClick, convertDate = TRUE),
  times=20)
