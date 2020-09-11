context('PAMrSettings object creation and manipulation')

test_that('Create and modify PRS', {
    exPrs <- new('PAMrSettings')
    exPrs <- addDatabase(exPrs, system.file('extdata', 'Example.sqlite3', package='PAMr'))
    exPrs <- addBinaries(exPrs, system.file('extdata', 'Binaries', package='PAMr'))
    exClick <- function(data) {
        standardClickCalcs(data, filterfrom_khz = 0)
    }
    exPrs <- addFunction(exPrs, exClick, module = 'ClickDetector')
    exPrs <- addFunction(exPrs, roccaWhistleCalcs, module='WhistlesMoans')
    exPrs <- addFunction(exPrs, standardCepstrumCalcs, module = 'Cepstrum')

    expect_is(exPrs, 'PAMrSettings')
    expect_equal(length(exPrs@db), 1)
    expect_equal(length(exPrs@binaries$list), 3)
    expect_equal(length(exPrs@binaries$folder), 1)
    expect_equal(length(exPrs@functions), 3)

    exPrs <- removeFunction(exPrs, 1)
    expect_equal(length(exPrs@functions$ClickDetector), 0)
    exPrs <- removeDatabase(exPrs, 1)
    expect_equal(length(exPrs@db), 0)
    exPrs <- removeBinaries(exPrs, 1)
    expect_equal(length(exPrs@binaries$folder), 0)
    expect_equal(length(exPrs@binaries$list), 0)

    # calibration
    calFile <- system.file('extdata', 'calibration.csv', package='PAMr')
    # adding but function doesnt have calibration, hshouldnt fail
    exPrs <- addCalibration(exPrs, calFile=calFile, all=TRUE, units=3)
    expect_equal(length(exPrs@calibration[[1]]), 1)
    # add funciton with cal option, now should work
    calClick <- function(data, calibration=NULL) {
        standardClickCalcs(data, calibration=calibration, filterfrom_khz = 0)
    }
    exPrs <- addFunction(exPrs, calClick, module = 'ClickDetector')
    exPrs <- applyCalibration(exPrs, all=TRUE)
    expect_equal(formals(exPrs@functions$ClickDetector[[1]])$calibration,
                 calFile)
})
