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

# what event is a UID in
whereUID <- function(study, UID, quiet=FALSE) {
    UID <- as.character(UID)
    where <- sapply(UID, function(u) { #for each uid
        ix <- which(sapply(events(study), function(e) { #go through each event
            any(sapply(detectors(e), function(d) { #and every detector in that event
                u %in% d$UID
            }))
        }))
        if(length(ix) == 0) {
            return(NA)
        }
        ix
    }, USE.NAMES=TRUE, simplify=FALSE)
    whereNA <- is.na(where)
    if(!quiet && any(whereNA)) {
        warning('UID(s) ', paste0(UID[whereNA], collapse=', '),
                ' not found in any events.')
    }
    where
}

# match SR function
# data needs UTC, thats it
matchSR <- function(data, db, extraCols = NULL, safe=FALSE) {
    if(!file.exists(db)) {
        if(safe) return(NULL)
        stop('Database ', db, ' does not exist.')
    }
    if(!('UTC' %in% colnames(data)) ||
       !inherits(data$UTC, 'POSIXct')) {
        stop('Data must have a column "UTC" in POSIXct format.')
    }
    con <-dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    soundAcquisition <- dbReadTable(con, 'Sound_Acquisition') %>%
        mutate(UTC = pgDateToPosix(UTC),
               Status = str_trim(Status),
               SystemType = str_trim(SystemType)) %>%
        filter(Status=='Start') %>%
        arrange(UTC) %>%
        select_(.dots = c('UTC', 'sampleRate', extraCols)) %>%
        distinct() %>%
        data.table() %>% setkey(UTC)

    data <- data.table(data) %>%
        setkey(UTC)

    # This rolling join rolls to the first time before. Since we filtered to only starts, it goes back
    # to whatever the last Start was.
    data <- soundAcquisition[data, roll = TRUE] %>%
        data.frame()
    srNa <- which(is.na(data$sampleRate))
    if(length(srNa) == nrow(data)) {
        srReplace <- as.integer(
            readline(prompt = 'No Sample Rate found in SoundAcquisition table. Enter Sample Rate for this data:\n')
        )
        data$sampleRate[srNa] <- srReplace
    } else if(length(srNa) > 0) {
        # get mode
        mode <- which.max(tabulate(data$sampleRate[-srNa]))
        srChoice <- menu(title=paste0('Could not get Sample Rate for all detections from the "SoundAcquistion" table.',
                                      ' Should missing values be replaced with ', mode, '(value found in table).'),
                         choices = c('Yes', 'No (I want to enter my own SR)'))
        srReplace <- switch(srChoice,
                            '1' = mode,
                            '2' = readline(prompt='What Sample Rate should be used?\n'),
                            stop('Sample Rate required for calculations.')
        )
        data$sampleRate[srNa] <- srReplace
    }
    data
}

# add list without replacing old one, only replace matching names
safeListAdd <- function(x, value) {
    if(!is.list(value) ||
       is.null(names(value))) {
        stop('Can only add named lists ')
    }
    hasName <- names(value) %in% names(x)
    if(any(hasName)) {
        for(n in names(value)[hasName]) {
            x[[n]] <- value[[n]]
        }
    }
    if(any(!hasName)) {
        x <- c(x, value[!hasName])
    }
    x
}
