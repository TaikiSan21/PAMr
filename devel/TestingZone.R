# Anne Test
anneList <- list.files('./devel/AnneTest/Binaries', full.names=TRUE, pattern='pgdf', recursive = TRUE)
anneDb <- './devel/AnneTest/SealBomb_HARP_test.sqlite3'

anneFuns <- list('ClickDetector'=list(binSpec))
anneAcev <- getDetectionsFromDb(anneDb, anneList, anneFuns)

test %>% group_by(DetectionNumber, Channel, Type) %>%
  mutate(br=(Bearing-median(Bearing) %% 360),
         br = ifelse(br > 180, br-180, br)) %>%
  ggplot(aes(x=br)) +
  # geom_histogram(binwidth=3) +
  geom_density(aes(color=Type)) +
  # facet_wrap(Channel~Type) +
  xlim(-180,180)

testData <- lapply(cl$data, function(x) {
  data.frame(UID = x$UID, N = sum(x$wave))
}) %>% bind_rows() %>% head()

cl$fileInfo$fileHeader$moduleName

testName <- lapply(cl$data[as.character(testData$UID)], function(x) {
  detName <- cl$fileInfo$fileHeader$moduleName
  data.frame(UID=x$UID,
             detectorName = unique(c(x$type, unlist(x$annotations)))) %>%
    mutate(detectorName = paste(detName, detectorName, sep='_'))
}) %>% bind_rows()

myDb <- './devel/ArrayTest/Database/testmuthafuckaaa929.sqlite3'
myList <- list.files('./devel/ArrayTest/TestBin', full.names=TRUE, pattern='pgdf', recursive=TRUE)
myFuns <- list('ClickDetector'=list(binSpec))
myDb <- './devel/ArrayTest/Database/PAM20010_HICEAS_Tracking_20170929.sqlite3'
myList <- list.files('./devel/ArrayTest/Binaries', full.names=TRUE, pattern='pgdf', recursive = TRUE)
myAcev <- getDetectionsFromDb(myDb, myList, myFuns)

## from all pascal data
bwPrs <- PAMrSettings()
bwData <- getPgDetections(bwPrs)
for(e in seq_along(bwData)) {
    species(bwData[[e]]) <-
        list('id' = str_trim(unique(detectors(bwData[[e]])[[1]]$eventType)))
    for(d in seq_along(detectors(bwData[[e]]))) {
        thisDet <- detectors(bwData[[e]])[[d]]
        detectors(bwData[[e]])[[d]] <- thisDet[!is.na(thisDet$peak),]
    }
}
bwLess <- bwData[sapply(bwData, function(e) !(species(e)$id %in% c('?BW', 'BWunid', 'OO')))]
bnt <- export_banter(bwLess)
names(bnt$detections) <- gsub(' ' , '', names(bnt$detections))
bntModel <- initBanterModel(bnt$events)
bntModel <- addBanterDetector(bntModel, bnt$detections, ntree = 1000, sampsize = 100)
bntModel <- runBanterModel(bntModel, ntree = 5000, sampsize = 2)
summary(bntModel)
