#' @title Show Graphical Representations of Waveforms
#'
#' @description Fetches matching binary data from a single or multiple
#'   detections in an \linkS4class{AcousticEvent} object, then plot
#'   the resulting data
#' 
#' @param acEv a \linkS4class{AcousticEvent} object
#' @param UID the UID(s) of the individual detections to fetch the binary 
#'   data for
#' @param \dots other arguments to pass to the spectrogram or wigner functions
#' 
#' @details The \code{showSpectrogram} function uses the function 
#'   \code{\link[signal]{specgram}} to plot the spectrogram, see this function
#'   for plotting options. The \code{showWigner} function uses the function
#'   \code{\link[PAMmisc]{wignerTransform}} to plot the Wigner-Ville transform,
#'   see this function for options.
#'   
#' @return Nothing, just shows plots for every channel of the waveform for
#'   each UID provided
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom signal specgram
#' @importFrom PAMmisc wignerTransform
#' @importFrom graphics plot title
#' @export
#'
showWaveform <- function(acEv, UID) {
    if(!('AcousticEvent' %in% class(acEv))) {
        stop('acEv must be an AcousticEvent class object.')
    }
    data <- getBinaryData(acEv, UID)
    for(i in seq_along(data)) {
        if(!('wave' %in% names(data[[i]]))) {
            warning('No waveform data found for UID ', names(data)[i])
            next
        }
        wav <- data[[i]]$wave
        for(c in 1:ncol(wav)) {
            plot(wav[, c], type='l')
            title(main = paste0('UID ', names(data)[i], ', Channel ', c))
        }
    }
}

#' @export
#' @rdname showWaveform
#' 
showSpectrogram <- function(acEv, UID, ...) {
    if(!('AcousticEvent' %in% class(acEv))) {
        stop('acEv must be an AcousticEvent class object.')
    }
    data <- getBinaryData(acEv, UID)
    sr <- sampleRate(settings(acEv))
    for(i in seq_along(data)) {
        if(!('wave' %in% names(data[[i]]))) {
            warning('No waveform data found for UID ', names(data)[i])
            next
        }
        wav <- data[[i]]$wave
        for(c in 1:ncol(wav)) {
            print(specgram(wav[, c], Fs = sr, ...))
            title(main = paste0('UID ', names(data)[i], ', Channel ', c))
        }
    }
}

#' @export
#' @rdname showWaveform
#' 
showWigner <- function(acEv, UID, ...) {
    if(!('AcousticEvent' %in% class(acEv))) {
        stop('acEv must be an AcousticEvent class object.')
    }
    data <- getBinaryData(acEv, UID)
    sr <- sampleRate(settings(acEv))
    for(i in seq_along(data)) {
        if(!('wave' %in% names(data[[i]]))) {
            warning('No waveform data found for UID ', names(data)[i])
            next
        }
        wav <- data[[i]]$wave
        for(c in 1:ncol(wav)) {
            wt <- wignerTransform(wav[, c], sr=sr, plot = TRUE)
            title(main = paste0('UID ', names(data)[i], ', Channel ', c))
        }
    }
}
