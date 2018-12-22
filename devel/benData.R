source('./devel/AziDataImport.R')
library(PAMsbuoy)
aziData <- processAziCsv('../Data/Azi/(SC)CCC_all.csv')
difarData <- readRDS('../Data/Azi/ccData.RData')
finalData <- matchToDifar(aziData, difarData)
calBuoy <- read.csv('../Data/Azi/calPositions.csv', stringsAsFactors = FALSE) %>%
    distinct(Longitude, Latitude, Buoy, Station)

calCur <- loadStations('../Data/CalCurCEAS/Database', extraCols='TrackedGroup', buoyPositions='../Data/CalCurCEAS/calPositions.csv')
calCombined <- calCur
names(calCombined) <- gsub('_P[123]', '', names(calCur))
calCombined <- PAMmisc::squishList(calCombined)
attr(calCombined, 'survey') <- '../Data/CalCurCEAS/Database'
for(s in seq_along(calCombined)) {
    attr(calCombined[[s]], 'station') <- names(calCombined)[s]
    for(b in seq_along(calCombined[[s]]$buoys)) {
        calCombined[[s]]$buoys[[b]]$info$buoyQuality <- ifelse(!is.null(calCombined[[s]]$buoys[[b]]$calibration),'Good', 'Bad')
    }
}

calibrateStations(calCombined, recalibrate = TRUE)

calDf <- bind_rows(lapply(calCombined, function(s) {
    ans <- s$detections
    if(nrow(ans) > 0) {
        ans$station <- attr(s, 'station')
    }
    ans
}))

calDf <- calDf %>%
    filter(Species == 'bmb',
           !grepl('S89S90', station)) %>%
    mutate(detection = gsub(' ', '', TrackedGroup))

calDf <- do.call(rbind, lapply(split(calDf, calDf$station), function(s) {
    whichNotNum <- s$detection == 'Nogroup'
    maxNum <- ifelse(sum(!whichNotNum) > 0, max(as.integer(s$detection[!whichNotNum])), 0)
    if(sum(whichNotNum) > 0) {
        s$detection[whichNotNum] <- maxNum + 1:(sum(whichNotNum))
    }
    s$detection <- as.integer(s$detection)
    s
}))

finalData <- matchToDifar(aziData, calDf)
calBuoy <- calBuoy %>%
    mutate(Station = gsub('_P[123]', '', Station))
benData <- left_join(finalData, calBuoy, by = c('Channel' = 'Buoy', 'Station')) %>%
    mutate(Station = gsub('\\.sqlite3', '', Station),
           Bearing = AziBearing + calibrationValue) %>%
    select(CallId, HydrophoneId = Channel, Bearing, Station, Latitude, Longitude, SignalAmplitude) %>%
    distinct()

# For Aaron
snrData <- readRDS('../Data/Azi/SNR_Data_All.RData') #ERRTHING
forAaron <- snrData %>%
    select(-NoiseSep, -NoiseTime, -CallTime, -ClipLength, -CallType, -Intensity,
           -Channel, -DIFARBearing, -Noise, -NoiseBearing) %>%
    rename(NoiseBearing = i.NoiseBearing)
pbAzi <- processAziCsv('../Data/Azi/Playback_SB_2016_final.csv')
dbDifar <- formatPlayback('../Data/Azi/DIFAR_single_buoy_308.csv')
pbFinal <- matchToDifar(pbAzi, dbDifar)

### Looking polar plot by station
