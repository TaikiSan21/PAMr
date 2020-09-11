context('Test processing data with a PRS')

test_that('Test process database', {
    exPrs <- new('PAMrSettings')
    exPrs <- addDatabase(exPrs, system.file('extdata', 'Example.sqlite3', package='PAMr'))
    exPrs <- addBinaries(exPrs, system.file('extdata', 'Binaries', package='PAMr'))
    exClick <- function(data) {
        standardClickCalcs(data, calibration=NULL, filterfrom_khz = 0)
    }
    exPrs <- addFunction(exPrs, exClick, module = 'ClickDetector')
    exPrs <- addFunction(exPrs, roccaWhistleCalcs, module='WhistlesMoans')
    exPrs <- addFunction(exPrs, standardCepstrumCalcs, module = 'Cepstrum')
    exData <- processPgDetections(exPrs, mode='db')

    expect_is(exData, 'AcousticStudy')
    expect_is(exData[1], 'AcousticStudy')
    expect_is(exData[[1]], 'AcousticEvent')
    expect_equal(length(detectors(exData[[1]])), 3)
    # check correct number of dets
    expect_equal(nrow(detectors(exData[[1]])[[1]]), 2)
    expect_equal(nrow(detectors(exData[[1]])[[2]]), 5)
    expect_equal(nrow(detectors(exData[[1]])[[3]]), 7)
    # check no NAs in calcs
    expect_true(!any(
        is.na(detectors(exData[[1]])[[1]]$peak)
    ))
    expect_true(!any(
        is.na(detectors(exData[[1]])[[2]]$ici)
    ))
    expect_true(!any(
        is.na(detectors(exData[[1]])[[3]]$freqBeg)
    ))
})

