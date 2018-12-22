# Normal spectrogram
library(tuneR)
library(seewave)
clickBin <- loadPamguardBinaryFile(testClick)
thisWave <- clickBin$data$`805002560`$wave[,1]
sr <- 192e3; wl <- 512
spectrogram <- seewave::spec(thisWave, f=sr, wl=fftSize, norm=FALSE, correction='amplitude', plot=FALSE)
relDb <- 20*log10(spectrogram[,2])
relDb[!is.finite(relDb)] <- NA

# Calibration
newClick <- data.frame(Freq=spectrogram[,1]*1e3, Sens = relDb)
# Get calibration data and fit gam
HP92Cal <- read.csv('./devel/BW/Frequency response of HTI-92 hydrophone from SM3M manual.csv')
gam92 <- gam(Sensitivity~s(Freq, 50), data=HP92Cal)

predictedValue <- predict.Gam(gam92, newdata=newClick)
# Normalize to level at 1khz
predictedValue <- predictedValue - predictedValue[1]
# Apply calibration and normalize
clickSens <- relDb-predictedValue
clickSens <- clickSens - max(clickSens)

library(gam)
# building is 99.99% of the time.
microbenchmark(
  gam = gam(Sensitivity~s(Freq, 50), data=HP92Cal),
  gampred = {
    gam92 <- gam(Sensitivity~s(Freq, 50), data=HP92Cal)
    predict.Gam(gam92, newData=newClick)
  },
  times=10000)
