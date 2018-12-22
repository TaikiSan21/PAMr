# New Faster Q from seewave
# Why are we eliminating the frequency turning into range?
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
                  type = "l", ...) {

  if (!is.null(f) & mel) {
    f <- 2 * mel(f/2)
  }
  if (is.null(f)) {
    if (is.vector(spec))
      stop("'f' is missing")
    else if (is.matrix(spec))
      f <- spec[nrow(spec), 1] * 2000 * nrow(spec)/(nrow(spec) - 1)
  }
  if (is.matrix(spec)) {
    range <- c(spec[1,1], spec[nrow(spec),1]) ### THERES NO RANGE IF SPEC IS VEC
    # spectest <- spec
    spec <- spec[, 2]
  }

  if (max(spec) == 1)
    stop("data must be in dB")
  if (which.max(spec) == 1)
    stop("maximal peak cannot be the first value of the spectrum")

  n1 <- length(spec)
  level2 <- max(spec) + level
  f0 <- which.max(spec)
  f0khz <- (((f0-1)/(n1-1)))*(range[2]-range[1]) + range[1] # These and others below need -1s to properly scale
  specA <- spec[1:f0]
  specB <- spec[f0:length(spec)]
  negA <- which(specA <= level2)
  if(length(negA) == 0) {
    fA <- 1
  } else {
    firstNegA <- max(which(specA <= level2)) # btwn this and next
    fA <- approx(x=spec[firstNegA:(firstNegA+1)], y=firstNegA:(firstNegA+1), xout=level2)$y
  }
  fAkhz <- ((fA-1)/(n1-1)) * (range[2]-range[1]) + range[1]
  nA <- length(specA)
  negB <- which(specB <= level2)
  if(length(negB) == 0) {
    fB <- length(spec)
  } else {
    firstNegB <- min(negB) + (nA - 1) # btwn this and prev
    fB <- approx(x=spec[(firstNegB-1):firstNegB], y=(firstNegB-1):firstNegB, xout=level2)$y
  }
  fBkhz <- ((fB-1)/(n1-1)) * (range[2]-range[1]) + range[1]
  Q <- f0khz/(fBkhz-fAkhz)
  results <- list(Q = Q, dfreq = f0khz, fmin = fAkhz, fmax = fBkhz,
                  bdw = fBkhz - fAkhz)

  results <- lapply(results, function(x) ifelse(length(x)==0, NA, x)) # Temp fix on missing
  if (plot) {
    if (is.null(flab)) {
      if (mel)
        flab <- "Frequency (kmel)"
      else flab <- "Frequency (kHz)"
    }
    x <- seq(range[1], range[2], length.out = n1)
    plot(x = x, y = spec, xlab = flab, ylab = alab, type = type,
         ...)
    arrows(fAkhz, level2, fBkhz, level2, length = 0.1, col = colval,
           code = 3, angle = 15)
    text(paste("Q =", as.character(round(Q, 2))), x = fBkhz,
         y = level2, pos = 4, col = colval, cex = cexval,
         font = fontval)
    invisible(results)
  }
  return(results)
}
