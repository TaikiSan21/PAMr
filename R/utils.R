# random utils

pgDateToPosix <- function(x) {
    as.POSIXct(as.character(x), format='%Y-%m-%d %H:%M:%OS', tz='UTC')
}