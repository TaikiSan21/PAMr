# BP paper synthetic BP creation notes

# Per mms paper tursiops burst pulses are:
# 2.2ms ICI, 60us click length, peak 38kHz

library(PAMmisc)
fn <- 'TestBP.wav'
od <- './devel'
sl <- 10 #seconds
cl <- 60 #micro sec
cps <- 1 / (2.2e-3)
f <- 20e3
sr <- 100e3
s <- c(0,0)
gf <- 0.1

writeClickWave(fileName = fn, outDir = od, signalLength = sl, 
               clickLength = cl, clicksPerSecond = cps, frequency = f, 
               sampleRate = sr, silence = s, gainFactor = gf)

bpWav <- createClickWave(signalLength = sl, clickLength = cl, clicksPerSecond = cps,
                         frequency = f, sampleRate = sr, silence = s, gainFactor = gf)
bpNoise <- tuneR::noise('white', duration = length(bpWav), samp.rate = sr, xunit = 'samples') %>% 
    tuneR::normalize(unit = '16', pcm = TRUE, level = .05)
plotLen <- 5e3
plot(bpWav@left[1:plotLen], type='l', col='blue')
lines(bpNoise@left[1:plotLen], col='red')
bpAll <- bpWav + bpNoise

doCeps <- function(signal) {
    Re(fft(log10(abs(fft(signal))^2 + 1e-3), inverse=TRUE))^2
}

doCepstro <- function(signal, wl = 1024, hop = 512) {
    binStart <- seq(1, length(signal), by = hop)
    extSignal <- c(signal, rep(0, wl))
    sapply(binStart, function(x) {
        doCeps(extSignal[x:(x+wl-1)])
    })
}

plotCepstro <- function(signal, sr, wl = 1024, hop = 512, skip = 1) {
    cepstro <- doCepstro(signal, wl, hop)[-skip, ]
    numLabs <- 10
    yLab <- round(seq(skip+1, wl, length.out = numLabs) / sr, 5)
    yVal <- seq(0, 1, length.out = numLabs)
    xLab <- round(seq(0, ncol(cepstro), length.out = numLabs) * hop / sr, 2)
    xVal <- seq(0, 1, length.out = numLabs)
    
    image(t(cepstro), xaxt = 'n', yaxt = 'n', col = gray.colors(256, start=.8, end=0))
    axis(1, at=xVal, labels = xLab)
    axis(2, at=yVal, labels = yLab)
}
plotCepstro(bpWav@left[1:5e3], bpWav@samp.rate)
plotCepstro(bpAll@left[1:50e3], bpAll@samp.rate)
hmm <- doCepstro(bpWav@left[1:5e3]) 
plot(hmm[, 1], type='l')
lines(x=rep(1/cps*sr, 2), y=c(0, 5e4), col='red')
