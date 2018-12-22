# Test BP processing
library(tuneR)
library(dtt)
window <- 512; overlap <- window/2

# This should be manual cepstral coeffs?
fftest <- fft(testWave@left[1:1024])
num <- length(fftest)/2
sr <- testWave@samp.rate
pow <- abs(fftest)^2+1
spectest <- fft(log(pow), inverse=TRUE)
xax <- (1:num)[-1]*sr/num/2
cepmax <- sort(Mod(spectest)[1:num], index.return=TRUE, decreasing=TRUE)$ix[1:10]
cepax <- sr/cepmax
plot(x=xax, y=pow[1:num][-1], type='l')
plot(x=1:(num-1), y=spectest[1:num][-1], type='l', xaxt='n') +
  axis(side=1, at=cepmax[-1], labels=cepax[-1]) #xlab is samples, in time should be x/SR
which(Mod(spectest) > 250)
# Holy shit in this example it finds exactly the click rate. It should kind of technically be finding
# repetition within your signal? So should be at harmonics.


cep <- melfcc(testWave, lifterexp = 0, wintime=window/sr, hoptime=overlap/sr, numcep=30)
image(cep[,2:30],col=gray.colors(256, start=.9, end=0))

spec <- periodogram(testWave, width=window, overlap=overlap, normalize=TRUE, channel='left', frqRange=c(3e3, 24e3))
print(image(spec, col=gray.colors(256, start=.7, end=0), ylim=c(0, 24e3), xaxt='n') +
        title(main=paste0('fr: ', sr/window, '. tr: ', window/sr, '. ici: ', 1/cps,
                          '. ratio: ', sr/cps/window)) +
        axis(side=1, at=seq(0, 200e3, 50e3), labels=seq(0, 200e3, 50e3)/sr))

specTest <- function(wave, window=512, nLabs=5, method='idft', lowfreq=1e3, skip=3e3) {
  sr <- wave@samp.rate
  butter <- signal::butter(2, lowfreq/(sr/2), type='high')
  filtdat <- signal::filter(butter, wave@left)
  extend <- c(filtdat, rep(0, window - (length(wave) %% window)))
  overlap <- window/2
  id <- 1
  switch(method,
         idft = {
           num <- window/2
           FUN <- function(x) {
             fft(x, inverse=TRUE)
           }
           skipVec <- 1:floor(sr/skip)
           xax <- (1:num)[-skipVec]*sr/num/2
         },
         dct = {
           num <- window
           FUN <- function(x) {
             dct(x)
           }
           skipVec <- 1:(floor(sr/skip)*2)
           xax <- (1:num)[-skipVec]*sr/num
         }
  )
  for(i in 1:(length(extend)/overlap-1)) {
    # for(i in 1:10) {
    thisfft <- fft(extend[id:(id+window-1)])
    if(any(is.na(thisfft))) next
    thispow <- abs(thisfft)^2+1
    if(i==1) {
      powsum <- thispow
      specsum <- abs(FUN(log(thispow)))
      specall <- specsum
    } else {
      # browser()
      powsum <- powsum + thispow
      thisSpec <- abs(FUN(log(thispow)))
      specsum <- specsum + thisSpec
      specall <- rbind(specall, thisSpec)
    }
    id <- id + overlap
  }
  cepmax <- sort(Re(specsum)[1:num][-skipVec], index.return=TRUE, decreasing=TRUE)$ix[1:nLabs]+length(skipVec)-1
  cepax <- switch(method,
                  idft = sr/cepmax,
                  dct = 2*sr/cepmax
  )
  powplot <- plot(x=xax, y=powsum[1:num][-skipVec], type='l')
  cepplot <- plot(x=(1:num)[-skipVec], y=specsum[1:num][-skipVec], type='l', xaxt='n') +
    axis(side=1, at=cepmax, labels=round(cepax, 0)) + abline(h=c(0, max(specsum[1:num][-skipVec])), col=c('black','red'))
  #xlab is samples, in time should be x/SR
  list(SpecSum=specsum, CepAx=cepax, PowPlot=powplot, CepPlot=cepplot, SpecAll=specall, skipVec=skipVec)
}


testFile <- './devel/Recording/LissoExample.wav'
testFile <- './devel/Recording/PilotExample.wav'
testFile <- './devel/Recording/OrcaExample.wav'
testFile <- './devel/Recording/BoingExample.wav'
testWave <- readWave(testFile, from = 139.354, to = 139.646, units = 'seconds')
# testWave <- readWave(testFile, from = 1.711, to = 2.647, units = 'seconds')
testWave <- Wave(left=testWave@left, samp.rate=testWave@samp.rate, bit=16, pcm=TRUE)

skip <-2000e3; window <- 1024*2
test <- specTest(testWave, method='idft', window=window, lowfreq=0e3, skip=skip, nLabs=20)
test$CepAx
44100*44100/2048/test$CepAx
image(test$SpecAll[,44:800], col=gray.colors(256, start=1, end=0))

atBin <- c(38, 310, 620)
plot(x=(1:1024)[-(1:20)], y=runAvg(test$SpecSum[1:1024][-(1:20)],1), type='l', xaxt='n',
     xlab='Bin Number (ICI = BinNum/SampleRate)', ylab='Cepstrum Magnitude', main='Cepstrum Peak Values') +
  # axis(side=1, at=44100/test$CepAx, labels=44100/test$CepAx)
  axis(side=1, at=atBin, labels=paste0(atBin, ' (', round(atBin/44100, 5), 's)'))
  # abline(h=0, col='black')

nLabs <- 10
allcep <- matrix(0, 13, nLabs)
for(i in 1:nrow(test$SpecAll)) {
  cepmax <- sort(Re(test$SpecAll)[i,][-test$skipVec], index.return=TRUE, decreasing=TRUE)$ix[1:nLabs]+length(test$skipVec)-1
  method <- 'dct'
  sr <- 44100
  cepax <- switch(method,
                  idft = sr/cepmax,
                  dct = 2*sr/cepmax)
  allcep[i,] <- cepax
}

#### ORCA EXAMPLE ####
# 49 Clicks 77.307 - 77.712 = 120.98 CPS
# 15 clicks 99.566 - 99.616 = 300 CPS
# 47 clicks 117.586 - 117.949 = 129 CPS
# 41 clicks 139.354 - 139.646 = 140 CPS

#### LISSO PG SPECTROGRAM ####
# dB 65.5 - 115. .714 - .855


## CANT DETECT BANDS AT SOME POINT - FIRST N COEFFS ARE REALLY LARGE NEGATIVE
## NUMBERS SO A PEAK WILL NEVER BE VISIBLE
## Test on a bunch of different cep to see at what point we have ability to detect
## signals

## Some next stuff - Beaker Banter Outline shared Google doc
## Learn teh mafs

## PG will want to run regular WM detctor, cepstral in R on those regular whistles,
## then also new cepstral detctor from Doug in PG. Compare.
