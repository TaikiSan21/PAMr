# Testing EG click calcs
#####
library(seewave)
library(gam)
EGRDS <- readRDS('S:/2018_PASCAL.BANTER/DASBR/BWwaveform.frombinaries/Station-1_Soundtrap-A_62.RDS')
# ch0 in name testing
cnt <- 0
for(rds in list.files('S:/2018_PASCAL.BANTER/DASBR/BWwaveform.frombinaries/', full.names=TRUE)) {
  tmp <- readRDS(rds)
  print(basename(rds))
  if(length(tmp)==0) next
  cnt <- cnt + sum(sapply(tmp, function(x) sum(grepl('ch0', names(x)))))
}
wtf <- readRDS('S:/2018_PASCAL.BANTER/DASBR/BWwaveform.frombinaries/Station-10_Card-G_7.RDS')

myClick <- loadPamguardBinaryFile('./devel/BW/Click_Detector_Click_Detector_Clicks_20160827_120207.pgdf')$data
testClick <- myClick[[242]]; testClick$sampleRate <- 288e3
binSpec(testClick)
thisWave <- myClick[[304]]$wave[,1]
eg <- EGRDS$Click_Detector_Click_Detector_Clicks_20160827_120207.pgdf$`160`

sr <- 288e3
fftSize <- switch(as.character(sr),
                  '288000' = 736,
                  '256000' = 654,
                  '192000' = 456)

thisTk <- TKEO(thisWave, f=sr, M=1,plot=F)
tkEnergy <- thisTk[(.001*sr):length(thisWave),2]
tkDb <- 10*log10(tkEnergy-min(tkEnergy, na.rm=TRUE))
tkDb <- tkDb - max(tkDb, na.rm=TRUE)
tkDb[!is.finite(tkDb)] <- NA
# plot(tkDb, type='l')
noiseLevel <- median(tkDb, na.rm=TRUE)
# noiseLevel

noiseThresh <- quantile(thisTk[,2], probs=.4, na.rm=TRUE)*100
dur <- subset(thisTk, thisTk[,2] >= noiseThresh)
dur <- 1e6*(max(dur[,1])-min(dur[,1]))

thisSpec <- spec(thisWave, f=sr, wl=fftSize, norm=FALSE, correction='amplitude', plot=T)
relDb <- 20*log10(thisSpec[,2])
relDb[!is.finite(relDb)] <- NA

newClick <- data.frame(Freq=thisSpec[,1]*1e3, Sens = relDb)
# Calibration
# predValue <- predict.Gam(calFun[[chan]], newdata=newClick)
predValue <- predict.Gam(gam92, newdata=newClick)
predValue <- predValue - predValue[1]
clickSens <- relDb-predValue
clickSens <- clickSens - max(clickSens)

calibratedClick <- cbind(newClick$Freq/1e3, clickSens)

dbBW10 <- data.frame(Q(calibratedClick, f=sr, level=-10, plot=FALSE))
colnames(dbBW10) <- c('Q_10dB', 'PeakHz_10dB', 'fmin_10dB', 'fmax_10dB', 'BW_10dB')
dbBW10$centerHz_10dB <- dbBW10$fmax_10dB - (dbBW10$BW_10dB/2)

dbBW3 <- data.frame(Q(calibratedClick, f=sr, level=-3, plot=FALSE))
colnames(dbBW3) <- c('Q_3dB', 'PeakHz_3dB', 'fmin_3dB', 'fmax_3dB', 'BW_3dB')
dbBW3$centerHz_3dB <- dbBW3$fmax_3dB - (dbBW3$BW_3dB/2)
dbBW10
dbBW3


#####
# EG VERSION
#####
FFF=test2$Click_Detector_Click_Detector_Clicks_20160827_145207.pgdf$click55ch0

TKfunc.left= TKEO(FFF@left,f=sr,M=1,plot = FALSE)
# ignore first 0.001 sec to avoid start-up artifacts
TKenergy= TKfunc.left[(0.001*sr):length(TKfunc.left[,2]),2]   #second column in output is energy
n= length(TKenergy)
BinCount= 1:n # not used
# convert energy to dB scale (note multiplier is 10 for energy, 20 for sound pressure)
TKenergyDB= 10*log10(TKenergy-min(TKenergy,na.rm=TRUE))
# normalize to 0 dB max
TKenergyDB= TKenergyDB - max(TKenergyDB,na.rm=TRUE)
TKenergyDB[!is.finite(TKenergyDB)] <- NA

noiselevel=median(TKenergyDB,na.rm=TRUE)
if (noiselevel > -20) next

#Duration

#Find noise threshold by multiplying all energy above the 40% threshold by 100.
noisethreshold=quantile(TKfunc.left[,2],probs = .40, na.rm = TRUE)*100
#Subset the TK function for energy above the 40% threshold.
dur=subset(TKfunc.left,TKfunc.left[,2]>= noisethreshold)
#Subtract the max time value from the minimum time value for the click duration.
duration=1000000*(max(dur[,1])-min(dur[,1]))
duration
f=sr
bar=spec(FFF@left,f=f,wl=fftSize, norm = FALSE, correction = "amplitude", plot=F)
#Convert amplitude to relative dB.
reldB=(20*log10(bar[,2]))
#Conver -Inf to NA
reldB[!is.finite(reldB)] <- NA

#Calibration Curve
newClick=data.frame(Freq=(bar[,1]*1000),Sensitivity = reldB)


#Apply the correct Calibration Curve
ClickNo <- 'click55ch0'
HTI92=c("Station-1","Station-3","Station-5","Station-6","Station-8","Station-9","Station-10","Station-12","Station-15","Station-16","Station-19","Station-22","Station-23","Station-24","Station-27-30")
HTI92ch0 <- c('Station-1')
ch092= ifelse(HTI92ch0 %in% HTI92,"T","F")
ch=ifelse(grepl("ch0",ClickNo, fixed=T),"T","F")
if(ch=="T" & ch092=="T") {
  PredHP92Values= predict.Gam(gam92,newdata=newClick)
  # normalize values relative to value at 1kHz
  PredHP92Values= PredHP92Values - PredHP92Values[1]
  clicksens=reldB-PredHP92Values
}
if(ch=="F" | ch092=="F") {
  PredHP96Values= predict.Gam(gam96,newdata=newClick)
  # normalize values relative to value at 1kHz
  PredHP96Values= PredHP96Values - PredHP96Values[1]
  clicksens=reldB-PredHP96Values
}

#Adjust so the peak is at 0.
Adj4zero=clicksens-max(clicksens)
TotCalibr= cbind(newClick$Freq/1000,Adj4zero)


dbBW10=Q(TotCalibr,f=f,level=-10,plot = FALSE)
DS=as.data.frame(t(unlist(dbBW10)))
names(DS)=c("Q_10db", "PeakHz_10dB", "fmin_10db","fmax_10db", "BW_10db")
DS$centerHz_10db=DS$fmax_10db-(DS$BW_10db/2)

dbBW3=Q(TotCalibr,f=f,level=-3, plot = FALSE)
temp=as.data.frame(t(unlist(dbBW3)))
names(temp)=c("Q_3db", "PeakHz_3dB", "fmin_3db","fmax_3db", "BW_3db")
temp$centerHz_3db=temp$fmax_3db-(temp$BW_3db/2)
#####
# Testing speed of click calculations
#####
testBin <- loadPamguardBinaryFile('./devel/BW/Click_Detector_Click_Detector_Clicks_20160909_192146.pgdf')$data[[88]]
testBin$sampleRate <- 288e3
calFuns <- pascalCalFun('Station-12')
binSpec(testBin,calFun = calFuns, new=T)
# GG PK: 38.8, 30.5, 25.5, 22.4  TR: 20, 28, 36

library(microbenchmark)
microbenchmark(
        new = binSpec(testBin, calFun=calFuns, new=TRUE, fast=FALSE),
        old = binSpec(testBin, calFun=calFuns, new=FALSE),
        newnew = binSpec(testBin, calFun=calFuns, new=TRUE, fast=TRUE),
        times=20) # originally 370 - 1.5. Down to 143 - 1.3 after changing Qtest. 83 to 28 there.
                  # Good lord, the stupids. Downt to 400:14 after fixing the nonsense in Q.

tmp <- tempfile()
Rprof(tmp, line.profiling=TRUE)
source('./devel/testrprof.R')
Rprof(NULL)
summaryRprof(tmp, lines='show') # currently 54% 122, 19% 133

test <- runif(1e5)
microbenchmark(
  which = which.max(test),
  other = max(test),
  times=1e5)


thisSpec <- spec(testBin$wave[,1], f=testBin$sampleRate, wl=736,norm=FALSE, correction='amplitude', plot=FALSE)
relDb <- 20*log10(thisSpec[,2])
relDb[!is.finite(relDb)] <- NA
relDb <- relDb - max(relDb, na.rm=TRUE)
calibratedClick <- cbind(thisSpec[,1], relDb)

microbenchmark(
  q = Q(calibratedClick, f=testBin$sampleRate, level=-10, plot=FALSE),
  qnew = Qtest(calibratedClick, f=testBin$sampleRate, level=-10, plot=FALSE),
  spec = spec(testBin$wave[,1], f=testBin$sampleRate, wl=736,norm=FALSE, correction='amplitude', plot=FALSE),
  times=30)
identical(Q(calibratedClick, f=testBin$sampleRate, level=-10, plot=FALSE), Qtest(calibratedClick, f=testBin$sampleRate, level=-10, plot=FALSE))

Q(calibratedClick, f=testBin$sampleRate, level=-10, plot=FALSE)
Qtest(calibratedClick, f=testBin$sampleRate, level=-10, plot=FALSE)
# level 2 was rounded to 1
original <- which(round(specB, 1) == level2)
rnd <- round(specB, 1)
microbenchmark(
  round = round(specB, 1),
  other = which((specB-level2 <= .05) & (specB-level2 >= -.05)),
  abs = which(abs(specB-level2) <= .05),
  which = which(rnd==level2),
  times=10)

specB <- as.matrix(spec1[f0:length(spec1)])
testB <- spec1[f0:length(spec1)]

microbenchmark(
  long = spec1 <- approx(spec, n = 1024000)$y,
  short = spec1 <- approx(spec, n = 10240)$y,
  times=1e3)
