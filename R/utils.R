# random utils

# for converting from database UTC columns that are characters
pgDateToPosix <- function(x) {
    as.POSIXct(as.character(x), format='%Y-%m-%d %H:%M:%OS', tz='UTC')
}

# drop columns with names cols
dropCols <- function(x, cols) {
    keepCols <- !(colnames(x) %in% cols)
    x[, keepCols, drop=FALSE]
}
