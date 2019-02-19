#' @title Remove a Database from a PAMrSettings Object
#'
#' @description Remove a database from the "db" slot in a PAMrSettings
#'   object.
#'
#' @param prs a \linkS4class{PAMrSettings} object to remove a database from
#' @param index index indicating which database(s) to remove. Can be a vector
#'   if you want to remove multiple databases. If missing user is prompted to
#'   select a database from a list, will only show up to the first 20. You can
#'   easily remove all of the databases with a large index like \code{1:1000}
#'
#' @return the same \linkS4class{PAMrSettings} object as prs, with the database(s)
#'   removed from the "db" slot
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom utils menu
#' @export
#'
removeDatabase <- function(prs, index=NULL) {
    if(is.null(index)) {
        choices <- prs@db
        if(length(choices) > 20) {
            warning('Only showing first 20 databases.')
            choices <- choices[1:20]
        }
        index <- menu(title = 'Choose a database to remove:',
                      choices = choices)
    }
    if(max(index) > length(prs@db)) warning('Index too large, no database to remove.')
    prs@db <- prs@db[-index]
    prs
}
