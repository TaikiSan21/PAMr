#' @title writeClickWave
#' @description Write a wave file for a synthesized delphinid click
#'
#' @param signalLength length of signal to create in seconds
#' @param clickLength length of each click in microseconds
#' @param clicksPerSecond number of clicks per second
#' @param frequency frequency of the clicks
#' @param sampleRate sample rate for the wave file to create
#' @param silence silence to pad before and after signal in seconds
#' @param gain scaling factor between 0 and 1. Low numbers are recommended (default 0.1)
#'
#' @details this code is based on Matlab code by Julie Oswald (2004)
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom tuneR Wave writeWave
#'
writeClickWave <- function(signalLength, clickLength, clicksPerSecond, frequency, sampleRate,
                           silence=c(0,0), gainFactor = .1) {
  clickLength <- clickLength / 1e6 # convert from micros
  clickPeriod <- 1 / clicksPerSecond
  if(clickPeriod < clickLength) {
    stop('Click Period is longer than Click Length')
  }
  t <- 0 : (clickLength * sampleRate)
  tone <- sin(2 * pi * frequency * t / sampleRate)
  gain <- exp(-t/16)
  # browser()
  tone <- tone * gain

  iciSilence <- clickPeriod - clickLength
  iciSilence <- rep(0, iciSilence * sampleRate)
  tone <- rep(c(tone, iciSilence), signalLength/clickPeriod)
  tone <- c(rep(0, silence[1]*sampleRate), tone, rep(0, silence[2]*sampleRate))
  filename <- paste0(signalLength, 's_', clickLength * 1e6, 'cl_', clicksPerSecond, 'cps_',
                     round(frequency / 1000, 0), 'k.wav')
  wav <- normalize(Wave(left=tone, samp.rate=sampleRate, bit=16), unit='16', level=gainFactor)
  # writeWave(wav, filename = filename, extensible = FALSE)
  wav
}
cl <- .5e2; cps <- 9e2; freq <- 20e3; sr <- 48e3; nClicks <- 200

# testing <- writeClickWave(nClicks / cps, cl, cps, freq, sr)
testing <- writeClickWave(1, cl, cps, freq, sr, silence=c(0,0))
for(f in (2:5)*1e4) {
  nextWave <- writeClickWave(1, cl, cps, f, sr, silence=c(0,0))
  testing <- bind(testing, nextWave)
}
for(cp in c(90,40, 20)) {
  nextWave <- writeClickWave(.5, cl, cp, freq, sr)
  testing <- bind(testing,nextWave)
}

window <- sr*2^-6; overlap <- window/2
cep <- melfcc(testing, lifterexp = 0, wintime=window/sr, hoptime=overlap/sr, numcep=30)
image(cep[,2:30],col=gray.colors(256, start=.9, end=0))

spec <- periodogram(testing, width=window, overlap=overlap, normalize=TRUE)
print(image(spec, col=gray.colors(256, start=.6, end=0), ylim=c(0, 4*freq), xaxt='n') +
        title(main=paste0('fr: ', sr/window, '. tr: ', window/sr, '. ici: ', 1/cps,
                          '. ratio: ', sr/cps/window)) +
        axis(side=1, at=seq(0, 200e3, 50e3), labels=seq(0, 200e3, 50e3)/sr))

# This should be manual cepstral coeffs?
fftest <- fft(testing@left)
ns <- length(fftest)/2
pow <- abs(fftest)^2 + 1
spectest <- fft(log(pow), inverse=TRUE)[1:ns]
xs <- 1:ns
plot(x=xs[-1], y=pow[1:ns][-1], type='l') + abline(v=(30e3))
plot(x=xs[-1], y=spectest[-1], type='l') + abline(v=sr/cps, col='red') #xlab is samples, in time should be x/SR
# Holy shit in this example it finds exactly the click rate. It should kind of technically be finding
# repetition within your signal? So should be at harmonics.
