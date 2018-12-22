# BANTER EXAMPLES #

# Run these if you havent installed banter or rfpermute
# devtools::install_github('ericarcher/banter', build_vignettes = TRUE)
# devtools::install_github('EricArcher/rfPermute')

# Point this to wherever you put the file
source('./devel/loadBanterData.R')

# Load your data
# This is the folder where your databases are. The binaries should be in a folder inside this folder
# just called "Binaries".
dbDirectory <- './devel/BW'
dbDirectory <- 'S:/2018_PASCAL.BANTER'
# Error in 20160908_10G or 7D, same binary name

# Loading the data will take time.
binList <- if(grepl('PASCAL', dbDirectory)) {
  readRDS('../PamBinaries/binaryList.RData')
} else readRDS('binaryList.RData')
bbwData <- bbw_data(dbDirectory, binList=binList, skip=0)
# bbwData <- bbw_data('D:/PASCAL')

# These format into the 'events' and 'detectors' that banter wants
bbwEvents <- bbw_exportEvents(bbwData)
bbwDetections <- bbw_exportDetections(bbwData)

# Strip out ? and unid
bbwDataTrim <- lapply(bbwData, function(x) filter(x, !(eventType %in% c('?BW', 'BWunid'))))
bbwEvents <- bbw_exportEvents(bbwDataTrim)
bbwDetections <- bbw_exportDetections(bbwDataTrim)

# Strip out less frequenct species
bbwFiltSpecies <- lapply(bbwData, function(x) filter(x, eventType %in% c('BB', 'BW34-50', 'BW43', 'BW46', 'GG', 'ZC')))
bbwEvents <- bbw_exportEvents(bbwFiltSpecies[1:7]) # only 1:6 had enough species in detector with BB,43,46,ZC
bbwDetections <- bbw_exportDetections(bbwFiltSpecies[1:7])

# Start banter model, add detectors, then run the model.
model <- initBanterModel(bbwEvents)
model <- addBanterDetector(model, bbwDetections, ntree=500, sampsize=30)
model <- runBanterModel(model, ntree=500, sampsize=15)

# Check NAs
# sapply(bbwData, function(d) which(apply(d, 1, function(x) sum(is.na(x)))>0))

# Hopefully it worked
summary(model)

# Stuff Can Remove
select(-UTC, -BinaryFile, -ClickNo, -eventType, -type, -db)

dropDumb <- c('duration', 'sampleRate', 'chan1PeakHz_10dB', 'chan1PeakHz_3dB', 'chan2PeakHz_10dB', 'chan2PeakHz_3dB') # Peaks can be diff by lot cuz avg


#### RESULTS SUMMARY ####
# Filtered 0509 data, SS 15, 15: BB 96%, BW43 95%, BW46 100%, ZC 91%
# Filtered 0509 data, SS 5, 5: BB 80%, 34.50 87%, 43 89%, 46 96%, GG 50%, OO 83%, ZC 81%
# Filtered 0509 data, SS 1, 1: BB 85%, 34.50 80%, 43 68%, 46 61%, 50.75 75%, GG 50%, NBHF 100%, OO 33%, ZC 62%

# 0515 w/o peak data, SS 15, 15: BB 92%, 34.50 90%, 43 89%, 46 91%, GG 69%, ZC 85%
# 0515 w/o peak data, SS 5, 5: BB 84%, 34.50 90%, 43 89%, 46 96%, GG 56%, OO 83%, ZC 78%
# 0515 w/o peak data, SS 1, 1: BB 91%, 34.50 70%, 43 68%, 46 69%, 50.75 100%, GG 69%, NBHF 100%, OO 50%, ZC 52%

# Filtered 0515 data, SS 15, 15: BB 91%, 34.50 95%, 43 89%, 46 96%, GG 69%, ZC 87%
# Filtered 0515 data, SS 5, 5: BB 83%, 34.50 95%, 43 92%, 46 96%, GG 56%, OO 83%, ZC 79%
# Filtered 0515 data, SS 1, 1: BB 91%, 34.50 80%, 43 68%, 46 69%, 50.75 100%, GG 58%, NBHF 100%, OO 50%, ZC 57%

# 0516 data
# BB 34.50 43 46 GG ZC Only SS 15, 15: BB 92%, 34.50 95%, 43 89%, 46 93%, GG 75%, ZC 86%
# BB 34.50 43 46 GG ZC Only SS 1, 1: BB 93%, 34.50 85%, 43 84%, 46 95%, GG 50%, ZC 68%
# BB 34.50 43 46 GG ZC Only SS 30, 1: BB 92%, 34.50 85%, 43 89%, 46 96%, GG 63%, ZC 78%
# BB 34.50 43 46 GG ZC Only SS 100, 15: BB 93%, 34.50 95%, 43 92%, 46 93%, GG 88%, ZC 90%

# BB 43 46 ZC Only SS 15: 93%, 95%, 93%, 92%
# BB 43 46 ZC Only SS 1: 92%, 79%, 96%, 74%


# Default is to split different detectors by their "Type" column (the 30-50khz etc. that they called it)
# Can also specify frequency bins. This will group them by peak frequency into the bins you specify here.
# The detectors will be labelled just 1,2,3,4,5, etc. in order of how many bins you make.
# Right now it uses peak from channel 1, you can change this in the 'pascalTypeSplit' function in the other file.
# Feel free to change these numbers.

freqBins <- c(0,20e3,40e3,60e3,80e3,120e3)
bbwData <- bbw_data(dbDirectory, binList=binList, freqBins = freqBins) # Then re-run lines 19-28 above

# If you want to add more functions / calculations, see the "binSpec" function in the other file. Currently
# it is only filtering out frequencies under 15khz, then getting the peak frequency from whatever is left.

bbwData <- bbw_data(dbDirectory, binList=binList)
test <- bind_rows(bbwData)
pks <- select(test, eventType, chan1peak, chan1trough, chan1peak2, chan2peak, chan2trough, chan2peak2)
pks <- gather(pks, type, freq, -eventType) %>% mutate(type=gsub('chan[12]', '', type))
ggplot(pks, aes(x=freq, color=type)) + geom_density() + facet_wrap(~species)
pks %>%
  filter(grepl('ZC', eventType)) %>%
  ggplot(aes(x=freq, fill=type)) + geom_histogram(bins=40) + xlim(0, 100) + geom_vline(xintercept=43)


pks <- select(bind_rows(bbwData), chan1peak, chan1peak2, chan1trough, chan2peak,
              chan2peak2, chan2trough, chan1peak3, chan1trough2, chan2peak3, chan2trough2, species=eventType) %>%
  tidyr::gather(type, freq, -species) %>% mutate(channel=ifelse(grepl('1', type), 1, 2), type=gsub('chan[12]', '', type))

######## SHAN PLOTS ##############
ggplot(pks, aes(x=freq, color=type)) + geom_density(size=1.5) + facet_wrap(~species) +
  theme(panel.background = element_rect(fill='grey')) + scale_x_continuous(breaks=seq(0,100, 20), limits=c(0,100)) +
  labs(x='Frequency', y='Density', title='Distribution of Peaks and Troughs in Beaked Whale Calls') +
  scale_color_hue(labels=c('Highest Peak', '2nd Peak', '3rd Peak', 'Trough / Notch', 'Trough / Notch')) +
  theme(plot.title=element_text(hjust=.5, size=16))

ggplot(data=bind_rows(bbwData), aes(color=as.factor(sampleRate))) + geom_point(aes(x=chan1trough, y=chan2trough))
ggplot(data=bind_rows(bbwData), aes(color=as.factor(sampleRate))) + geom_point(aes(x=chan1peak, y=chan2peak))
ggplot(data=bind_rows(bbwData), aes(color=as.factor(sampleRate))) + geom_point(aes(x=chan1peak2, y=chan2peak2))


#
testSpec <- spec(testBin$data[[6]]$wave[,2], f=288e3, plot=FALSE)
testSpec[,2] <- 20*log10(testSpec[,2])
peakTrough(testSpec, plot=T, avg=7)

splitChan <- lapply(bbwData, function(d) {
  chan1 <- d[, !grepl('chan2', colnames(d))] %>%
    mutate(Channel=1) %>% select(-duration)
  colnames(chan1) <- gsub('chan1', '', colnames(chan1))
  chan2 <- d[, !grepl('chan1', colnames(d))] %>%
    mutate(Channel=2) %>% select(-duration)
  colnames(chan2) <- gsub('chan2', '', colnames(chan2))
  rbind(chan1,chan2) %>% mutate(Id = Id + Channel*1e9) %>%
    select(-PeakHz_3dB, -Channel)
})

splitChan <- lapply(bbwData, function(d) {
  select(d, -sampleRate)
})

splitChanReal <- lapply(splitChan, function(x) filter(x, !(eventType %in% c('?BW', 'BWunid'))))
splitChanBW <- lapply(splitChan, function(x) filter(x, !(eventType %in% c('?BW', 'BWunid', 'GG', 'NBHF', 'OO'))))
splitChanUnid <- lapply(splitChan, function(x) filter(x, eventType %in% c('?BW', 'BWunid')))

saveRDS(splitChan, 'bbwData_all.RData')
saveRDS(splitChanBW, 'bbwData_BW.RData')
saveRDS(splitChanReal, 'bbwData_allid.RData')
saveRDS(splitChanUnid, 'bbwData_unid.RData')

######## SHAN PLOTS ##############
ggplot(pks, aes(x=freq, color=type)) + geom_density(size=1.5) + facet_wrap(~species) +
  theme(panel.background = element_rect(fill='grey')) + scale_x_continuous(breaks=seq(0,100, 20), limits=c(0,100)) +
  labs(x='Frequency', y='Density', title='Distribution of Peaks and Troughs in Beaked Whale Calls') +
  scale_color_hue(labels=c('Highest Peak', '2nd Peak', 'Trough / Notch'), name='Type') +
  theme(plot.title=element_text(hjust=.5, size=16))



