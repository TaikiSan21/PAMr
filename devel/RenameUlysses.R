# Parse file names
# Needs to end up DIFAR_YYYYMMDDTHHMMSS
oldDir <- getwd()
setwd(choose.dir())
oldNames <- list.files('.', pattern='wav')
# HICEAS sonobuoy pattern
# newNames <- gsub('sb_([0-9]{8})_([0-9]{6})_.*', 'DIFAR_\\1T\\2\\.wav', oldNames)
# CalCurCEAS sonobuoy pattern
newNames <- gsub('PAM_([0-9]{8})_([0-9]{6})', 'DIFAR_\\1T\\2', oldNames)
# oldNames <- 'PAM_20140806_030748.wav'
file.rename(from=oldNames, to=newNames)
setwd(oldDir)
print('Done!')
