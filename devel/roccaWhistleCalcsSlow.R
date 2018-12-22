#' @title Calculate a Set of Measurements for Whistles
#'
#' @description Calculate a set of measurements from a whistle contour. All
#'   calculations following ROCCA method from Julie and Michael Oswald, as
#'   implemented in Pamguard.
#'
#' @param data a list that must have \code{freq} the whistle contour stored as a
#'   vector of FFT bin frequencies in hertz, and \code{time} the time in seconds
#'   at each bin.
#'
#' @return A list with 50 calculated rocca parameters, each item in the list will
#'   only have 1 entry so that this can easily be converted to a data frame.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom stats sd
#' @export
#'
roccaWhistleCalcsSlow <- function(data) {
    # ALL SLOPE CALCS ARE VERY SLIGHTLY OFF???? NOT SURE HOW TO TEST
    # I think slope difference is from getTime() getting more accuracte /
    # granular times than just step size / SR.
    # Sweep frequency calcs are off by the sweeps set to default  in te
    # setSweep() function, is the Java doing what it should?? those shouldnt
    # be counted as sweeps of default type???
    neededVals <- c('freq', 'time')
    missingVals <- neededVals[!(neededVals %in% names(data))]
    if(length(missingVals) > 0) {
        stop('Values for', paste(missingVals, collapse=', '), 'are missing.',
             'These are required for Rocca Whistle Calculations, please fix.')
    }
    contour <- data.frame(freq = data$freq, time = data$time)
    nSlices <- nrow(contour)

    result <- data.frame(freqBeg = contour$freq[1],
                         freqEnd = contour$freq[nSlices],
                         freqMean = mean(contour$freq),
                         freqStdDev = sd(contour$freq),
                         duration = (contour$time[nSlices] - contour$time[1]))
    contour$sweep <- 0 # 0 1 2 down flat up
    contour$step <- 0 # 0 1 2 flat up down
    contour$slope <- 0
    contour$timediff <- 0
    inflTimeArray <- rep(NA, nrow(contour))
    for(i in 2:nSlices) {
        contour$timediff[i] <- contour$time[i]-contour$time[i-1]
        contour$slope[i] <- ifelse(contour$timediff[i]==0, 0,
                                   (contour$freq[i]-contour$freq[i-1])/contour$timediff[i])
        contour$step[i] <- setStep(contour, i, stepSens = 11)
        contour$sweep[i] <- setSweep(contour, i)
        # Inflection / Direction calcs Java 608-676
        if(i %in% 2:3) {
            # init at 3, 1st is not real sweep so compare to prev starts at i==3
            direction <- contour$sweep[2]
        } else if((direction == 0 & contour$sweep[i] == 2) ||
           (direction == 2 & contour$sweep[i] == 0)) {
            inflTimeArray[i] <- contour$time[i]
            direction <- contour$sweep[i]
        } else if(direction == 1) {
            direction <- contour$sweep[i]
        }
    }
    # Not first, not if time diff==0. First is 0. Java 749
    useSlope <- contour$slope[contour$timediff != 0]
    result$freqSlopeMean <- mean(useSlope)
    result$freqAbsSlopeMean <- mean(abs(useSlope))
    # positive slopes
    useSlope <- contour$slope[contour$slope > 0]
    result$freqPosSlopeMean <- ifelse(length(useSlope)==0, 0,
                                      mean(useSlope))
    # negative slopes
    useSlope <- contour$slope[contour$slope < 0]
    if(length(useSlope)==0) {
        result$freqNegSlopeMean <- 0
        result$freqSlopeRatio <- 0
    } else {
        result$freqNegSlopeMean <- mean(useSlope)
        result$freqSlopeRatio <- result$freqPosSlopeMean / result$freqNegSlopeMean
    }
    # This should be fine, check Java 494
    result$freqStepUp <- sum(contour$step==1)
    result$freqStepDown <- sum(contour$step==2)
    sweepType <- summary(factor(paste0(contour$sweep[2:(nSlices-1)],
                                          contour$sweep[3:nSlices]),
                                levels = c('00', '01', '02', '10', '11',
                                           '12', '20', '21', '22')))


    result$numSweepsDwnFlat <- sweepType[['01']]
    result$numSweepsDwnUp <- sweepType[['02']]
    result$numSweepsFlatDwn <- sweepType[['10']]
    result$numSweepsFlatUp <- sweepType[['12']]
    result$numSweepsUpDwn <- sweepType[['20']]
    result$numSweepsUpFlat <- sweepType[['21']]
    result$numInflections <- sum(!is.na(inflTimeArray))

    # Java 694
    freqCofmSum <- 0
    for(i in seq(7, nSlices, 3)) { # java starts at ix=6 for some reason
        freqCofmSum <- freqCofmSum + abs(contour$freq[i]-contour$freq[i-3])
    }
    result$freqCofm <- freqCofmSum / 10000

    freqQuarters <- contour$freq[round(c(1, 2, 3)*nSlices/4, 0)]
    result$freqQuarter1 <- freqQuarters[1]
    result$freqQuarter2 <- freqQuarters[2]
    result$freqQuarter3 <- freqQuarters[3]

    freqSort <- sort(contour$freq)
    result$freqSpread <- diff(quantile(freqSort, c(.25, .75)))
    result$freqMin <- freqSort[1]
    result$freqMax <- freqSort[nSlices]
    result$freqRange <- result$freqMax - result$freqMin
    result$freqMedian <- median(freqSort)
    result$freqCenter <- result$freqMin + result$freqRange/2
    result$freqRelBw <- result$freqRange / result$freqCenter
    result$freqMaxMinRatio <- result$freqMax / result$freqMin
    result$freqBegEndRatio <- result$freqBeg / result$freqEnd

    # Java 739
    result$freqNumSteps <- result$freqStepUp + result$freqStepDown
    result$stepDur <- result$freqNumSteps / result$duration

    # Java 770
    slopeBegAve <- mean(contour$slope[2:4]) # not first cuz 0
    if(slopeBegAve > 0) {
        result$freqBegSweep <- 2
        result$freqBegUp <- TRUE
        result$freqBegDwn <- FALSE
    } else if(slopeBegAve < 0) {
        result$freqBegSweep <- 0
        result$freqBegUp <- FALSE
        result$freqBegDwn <- TRUE
    } else {
        result$freqBegSweep <- 1
        result$freqBegUp <- FALSE
        result$freqBegDwn <- FALSE
    }
    slopeEndAve <- mean(contour$slope[(nSlices-2):nSlices])
    if(slopeEndAve > 0) {
        result$freqEndSweep <- 2
        result$freqEndUp <- TRUE
        result$freqEndDwn <- FALSE
    } else if(slopeEndAve < 0) {
        result$freqEndSweep <- 0
        result$freqEndUp <- FALSE
        result$freqEndDwn <- TRUE
    } else {
        result$freqEndSweep <- 1
        result$freqEndUp <- FALSE
        result$freqEndDwn <- FALSE
    }

    # java 810
    useSweep <- contour$sweep[2:(nSlices-1)] # not first or last, they default to 0
    if(length(useSweep) > 0) {
        result$freqSweepUpPercent <- sum(useSweep == 2) / length(useSweep) * 100
        result$freqSweepDwnPercent <- sum(useSweep == 0) / length(useSweep) * 100
        result$freqSweepFlatPercent <- sum(useSweep == 1) / length(useSweep) * 100
    } else { # just in case we dont have any, set to 0
        result$freqSweepUpPercent <- 0
        result$freqSweepDwnPercent <- 0
        result$freqSweepFlatPercent <- 0
    }
    # Java 857
    if(result$numInflections > 1) {
        inflOnly <- inflTimeArray[!is.na(inflTimeArray)]
        lastInfl <- inflOnly[result$numInflections]
        smallArray <- (lastInfl - inflOnly[-result$numInflections])
        smallArray <- sort(smallArray)
        result$inflMaxDelta <- smallArray[length(smallArray)]
        result$inflMinDelta <- smallArray[1]
        result$inflMaxMinDelta <- result$inflMaxDelta / result$inflMinDelta
        result$inflMeanDelta <- mean(smallArray)
        result$inflStdDevDelta <- ifelse(length(smallArray) > 1, sd(smallArray), 0) # 0 not NA for 1 ting
        result$inflMedianDelta <- median(smallArray)
        result$inflDur <- result$numInflections / result$duration
    } else {
        result$inflMaxDelta <- 0
        result$inflMinDelta <- 0
        result$inflMaxMinDelta <- 0
        result$inflMeanDelta <- 0
        result$inflStdDevDelta <- 0
        result$inflMedianDelta <- 0
        result$inflDur <- 0
    }
    result
}

# I dont think Java comment is correct on steps covering multiple time steps 494
# This always resets to flat
setStep <- function(contour, ix, stepSens = 11) {
    # 0 1 2 flat up down
    if(contour$step[ix-1] == 0 &&
       contour$freq[ix] >= contour$freq[ix-1]*(1+stepSens/100)) {
        return(1)
    }
    if(contour$step[ix-1] == 0 &&
       contour$freq[ix] <= contour$freq[ix-1]*(1-stepSens/100)) {
        return(2)
    }
    0
}

setSweep <- function(contour, ix) {
    # JAVA updating these sweep counts individually, and not if we make it
    # to the default to last scenario which is a v or ^ frequency

    # Last and first dont get set?
    # I think PG default is 0, poss should be prev
    if(ix == nrow(contour)) {
        return(0)
        # return(contour$sweep[ix-1])
    }
    # 0 1 2 down flat up
    # flat-flat is flat
    if(contour$freq[ix-1] == contour$freq[ix]  &&
       contour$freq[ix] == contour$freq[ix+1]) {
        return(1)
    }
    # up-up, up-flat, flat-up are up
    if(contour$freq[ix-1] <= contour$freq[ix] &&
       contour$freq[ix] <= contour$freq[ix+1]) {
        return(2)
    }
    # down-down, flat-down, down-flat are down
    if(contour$freq[ix-1] >= contour$freq[ix] &&
       contour$freq[ix] >= contour$freq[ix+1]) {
        return(0)
    }
    # default to last
    contour$sweep[ix-1]

}

setInflection <- function(contour, ix) {
    # 0 1 2 down flat up sweep
    if(ix == 2) {
        return(contour$sweep[1])
    }
    if((contour$direction[ix-1] == 0 & contour$sweep[ix] == 2) ||
       (contour$direction[ix-1] == 2 & contour$sweep[ix] == 0)) {

        return(contour$sweep[ix])
    }
    contour$direction[ix-1]
}
