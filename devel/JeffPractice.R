# Jeff R practice
library(stringr)
library(dplyr)
library(data.table)

# Source me, then do processAziCsv('pathToMyFile')

processAziCsv <- function(file) {
  if(missing(file)) {
    file <- file.choose()
  }
  if(!grepl('csv', file)) {
    stop('I want a csv')
  }
  aziData <- read.csv(file, stringsAsFactors = FALSE)
  chan <- gsub('Channel([0-9]*).*', '\\1', colnames(aziData))
  type <- gsub('.*_(.*)$', '\\1', colnames(aziData))
  result <- apply(aziData, 2, processAziColumn)
  for(i in seq_along(result)) {
    result[[i]]$Channel <- chan[i]
    result[[i]]$AziType <- type[i]
  }
  bind_rows(result) %>%
    mutate(Channel = as.integer(Channel))
}
processAziColumn <- function(aziCol) {
  aziCol <- str_trim(aziCol)
  aziCol <- aziCol[which(nchar(aziCol) > 0)]
  timeLocs <- grep('Absolute time', aziCol)

  start <- 1
  result <- list()
  for(i in seq_along(timeLocs)) {
    thisTime <- gsub('.*time:\\s*(.*)$', '\\1', aziCol[timeLocs[i]])
    thisTime <- as.POSIXct(thisTime, format = '%d-%b-%Y %H:%M:%S', tz='UTC')
    thisDf <- processAziRow(aziCol[start:(timeLocs[i]-1)]) %>%
      mutate(AziTime = thisTime + AziTime,
             DetectionNumber = i)
    result[[i]] <- thisDf
    start <- timeLocs[i]+1
  }
  bind_rows(result)
}

processAziRow <- function(aziRow) {
  result <- list()
  splitAzi <- gsub('.*Point\\s([0-9]*).*Time:\\s([0-9\\.]*).*Frequency:\\s*([0-9\\.]*).*Directions:\\s*([0-9\\.]*).*',
                   '\\1-\\2-\\3-\\4', aziRow) %>% str_split('-', simplify=TRUE)
  result$Point <- as.integer(splitAzi[,1])
  result$AziTime <- as.numeric(splitAzi[,2])
  result$AziTime <- result$AziTime - min(result$AziTime)
  result$AziFrequency <- as.numeric(splitAzi[,3])
  result$AziBearing <- as.numeric(splitAzi[,4])
  bind_rows(result)
}

formatPlayback <- function(playbackCsv) {
  data <- read.csv(playbackCsv, stringsAsFactors = FALSE) %>%
    mutate(UTC = as.POSIXct(UTC, format = '%Y-%m-%d %H:%M:%S', tz='UTC'),
           Channel = sapply(Buoy, function(x) switch(x,
                         'NE' = 1, 'SW' = 2, 'SE' = 3, 'NW' = 4), USE.NAMES = FALSE)) %>%
    select(-X, -posixDate, -Buoy)
  data
}

matchToDifar <- function(aziData, difarData) {
  result <- list()
  aziData <- aziData %>%
    group_by(DetectionNumber, Channel, AziType) %>%
    summarise(AziTime = median(AziTime, na.rm = TRUE),
              AziFrequency = median(AziFrequency, na.rm=TRUE),
              AziBearing = median(AziBearing, na.rm=TRUE)) %>%
    ungroup()
  # Ours start at 1
  if(0 %in% unique(difarData$Channel)) {
    difarData$Channel <- difarData$Channel + 1
  }
  difarData <- filter(difarData,
                      UTC < max(aziData$AziTime),
                      UTC > min(aziData$AziTime))
  for(i in unique(aziData$Channel)) {
    thisAziSignal <- filter(aziData, Channel==i, AziType == 'Signal') %>%
      select(AziTime, AziBearing, AziFrequency) %>% data.table(key = 'AziTime')
    thisDifar <- data.table(filter(difarData, Channel==i), key='UTC')
    thisDifar <- thisAziSignal[thisDifar, roll='nearest']
    thisNoise <- filter(aziData, Channel==i, AziType=='Noise') %>%
      select(AziTime, NoiseBearing = AziBearing) %>% data.table(key='AziTime')
    thisDifar <- thisNoise[thisDifar, roll='nearest']
    result[[i]] <- thisDifar
  }
  result <- bind_rows(result)
  if('RealBearing' %in% colnames(result)) {
    result %>%
      mutate(AziError = (RealBearing - AziBearing) %% 360,
             AziError = ifelse(AziError > 180, AziError - 360, AziError))
  } else {
    result
  }
}

finalData %>%
  filter(TrackedGroup == 'S1') %>%
  ggplot(aes(x=SignalAmplitude, shape=as.factor(Intensity))) +
  geom_point(aes(y=DifarAdj), color='red') +
  geom_point(aes(y=AziBearing), color='green') +
  geom_point(aes(y=NoiseBearing), color='blue') +
  facet_wrap(~Channel)

test %>%
  # filter(StationNumber == 'S5') %>%
  mutate(tonal = grepl('tone', CallType)) %>%
  ggplot(aes(y=30-SNR, shape=as.factor(Intensity))) +
  geom_point(aes(x=DifarBearing), color='red') +
  geom_point(aes(x=AziBearing), color='darkgreen') +
  geom_point(aes(x=NoiseBearing), color='blue') +
  geom_vline(aes(xintercept=RealBearing), color='green') +
  geom_vline(xintercept=seq(0,360,45), color='darkgrey') +
  geom_vline(xintercept=seq(22.5, 337.5, 45), color='darkgrey') +
  scale_x_continuous(labels=c(360,45,90,135,180,225,270,315), breaks=c(0,45,90,135,180,225,270,315), limits=c(0,360)) +
  coord_polar() +
  facet_wrap(tonal~Channel, nrow=2)

library(RSQLite)
library(lubridate)
noise <- bind_rows(
  lapply(list.files('../SonoBuoy/Data/SB', pattern='sqlite', full.names=TRUE), function(db) {
    con <- dbConnect(SQLite(), db)
    thisDf <- dbReadTable(con, 'DIFAR_Localisation')
    dbDisconnect(con)
    thisDf
  }))

noise <- {
    con <- dbConnect(SQLite(), './devel/NoiseDifferentBox10.sqlite3')
    thisDf <- dbReadTable(con, 'DIFAR_Localisation')
    dbDisconnect(con)
    thisDf
} %>%
  mutate(UTC = lubridate::ymd_hms(UTC),
         Species = str_trim(Species),
         Channel = Channel + 1)

aziData <- processAziCsv(file.choose()) #SB_playback in downloads
difarData <- formatPlayback(file.choose()) #difar_single buoy
finalData <- matchToDifar(aziData, difarData) %>%
  mutate(Species = paste0(CallType, Intensity),
         Buoy = Channel,
         UTC = CallTime,
         DIFARBearing = DifarBearing)
noiseDict <- data.frame(Species = c('dn1', 'dn5', 'DN5', 'dn8', 'upX1', 'upX5', 'upX8',
                                    'upY1', 'upY5', 'upY8', 'upZ1', 'upZ5', 'upZ8',
                                    'tone1', 'tone5', 'tone8'),
                        Noise = c(rep('ambientbig', 13), rep('ambientsmall', 3)))

snrData <- noiseMatcherJeff(finalData, noiseDict, noise)
#Line 9450 Ch2 Noise
