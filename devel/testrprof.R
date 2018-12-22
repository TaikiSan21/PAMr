# originally 132 60%, 121 20%, 128 12%
Qtest <- function(spec = calibratedClick,
                   f = testBin$sampleRate,
                   level = -10,
                   mel = FALSE,
                   plot = FALSE,
                   colval = "red",
                   cexval = 1,
                   fontval = 1,
                   flab = NULL,
                   alab = "Relative amplitude (dB)",
                   type = "l") {


  if (!is.null(f) & mel) {
    f <- 2 * mel(f/2)
  }
  if (is.null(f)) {
    if (is.vector(spec))
      stop("'f' is missing")
    else if (is.matrix(spec))
      f <- spec[nrow(spec), 1] * 2000 * nrow(spec)/(nrow(spec) -
                                                      1)
  }
  if (is.matrix(spec)) {
    range <- c(spec[1,1], spec[nrow(spec),1])
    spec <- spec[, 2]
  }

  if (max(spec) == 1)
    stop("data must be in dB")
  if (which.max(spec) == 1)
    stop("maximal peak cannot be the first value of the spectrum")
  n0 <- length(spec)
  spec1 <- approx(spec, n = 1024000)$y
  n1 <- length(spec1)
  level2 <- round(max(spec1), 1) + level
  f0 <- which.max(spec1)
  f0khz <- ((f0/n1) * (range[2] - range[1])) + range[1]
  specA <- spec1[1:f0]
  nA <- length(specA)
  specB <- spec1[f0:length(spec1)]
  f1 <- which(round(specA, 1) == level2)
  f1khz <- ((f1[length(f1)]/n1) * (range[2] - range[1])) +
    range[1]
  f2 <- which(abs(specB-level2) <= .05) + (nA - 1)
  f2khz <- ((f2[1]/n1) * (range[2] - range[1])) + range[1]
  #    Q <- f0/(f2[1] - f1[length(f1)])
  Q <- f0khz/(f2khz-f1khz)    ## JS modification
  results <- list(Q = Q, dfreq = f0khz, fmin = f1khz, fmax = f2khz,
                  bdw = f2khz - f1khz)
  if (plot) {
    if (is.null(flab)) {
      if (mel)
        flab <- "Frequency (kmel)"
      else flab <- "Frequency (kHz)"
    }
    x <- seq(range[1], range[2], length.out = n0)
    plot(x = x, y = spec, xlab = flab, ylab = alab, type = type,
         ...)
    arrows(f1khz, level2, f2khz, level2, length = 0.1, col = colval,
           code = 3, angle = 15)
    text(paste("Q =", as.character(round(Q, 2))), x = f2khz,
         y = level2, pos = 4, col = colval, cex = cexval,
         font = fontval)
    invisible(results)
  }
  return(results)
}
for(i in 1:50) {
  Qtest(calibratedClick, f=testBin$sampleRate, level=-10, plot=FALSE)
}


