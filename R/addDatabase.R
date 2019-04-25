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
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @export
#'
addDatabase <- function(prs, db=NULL) {
    if(is.null(db)) {
        cat('Please select a database file if you have one.',
            'Multiple selections are ok, or cancel if you do not.\n')
        db <- choose.files(caption='Select database(s):')
    }
    # Case when cancelled or some weirdness
    if(length(db) == 0) return(prs)

    exists <- file.exists(db)
    if(!all(exists)) {
        stop(paste0('Database ', db[!exists], ' does not exist'))
    }
    isSqlite <- grepl('\\.sqlite3$', db)
    if(any(!isSqlite)) {
        warning('Some files selected that are not sqlite3 databases,',
                ' these files have been removed from the selection: ',
                paste0(db[!isSqlite], collapse = ', '))
        db <- db[isSqlite]
    }
    prs@db <- unique(c(prs@db, db))
    prs
}
