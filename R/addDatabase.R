#' @title Add a Database to a PAMrSettings Object
#'
#' @description Adds a new function to the "function" slot in a PAMrSettings
#'   object.
#'
#' @param prs a \linkS4class{PAMrSettings} object to add a database to
#' @param db a database to add
#'
#' @return the same \linkS4class{PAMrSettings} object as prs, with the database
#'   \code{db} added to the "db" slot
#'
#' @examples
#'
#' # not recommended to create a prs like this, for example only
#' prs <- new('PAMrSettings')
#' db <- system.file('extdata', 'Example.sqlite3', package='PAMr')
#' prs <- addDatabase(prs, db)
#' prs
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @export
#'
addDatabase <- function(prs, db=NULL) {
    if(is.PAMrSettings(db)) {
        db <- db@db
    }
    if(is.null(db)) {
        cat('Please select a database file,',
            ' multiple selections are ok..\n')
        db <- choose.files(caption='Select database(s):')
    }
    # Case when cancelled or some weirdness
    if(length(db) == 0) return(prs)

    exists <- file.exists(db)
    if(any(!exists)) {
        warning(paste0('Database(s) ',
                       paste0(db[!exists], collapse=', ')
                       , ' do(es) not exist.'))
        db <- db[exists]
    }
    isSqlite <- grepl('\\.sqlite3$', db)
    if(any(!isSqlite)) {
        warning('Some files selected that are not sqlite3 databases,',
                ' these files have been removed from the selection: ',
                paste0(db[!isSqlite], collapse = ', '))
        db <- db[isSqlite]
    }
    cat('Adding', length(db), 'databases:\n  ')
    if(length(db) > 6) {
        dbMsg <- paste0(c(basename(db[1:6]),paste0('... (', length(db)-6, ' more not shown)')), collapse = '\n  ')
    } else {
        dbMsg <- paste0(basename(db), collapse=', ')
    }
    cat(dbMsg, '\n')
    prs@db <- unique(c(prs@db, db))
    prs
}
