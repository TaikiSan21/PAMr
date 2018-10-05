#' @title Remove a Function from a PAMrSettings Object
#'
#' @description Remove a function from the "function" slot in a PAMrSettings
#'   object.
#'
#' @param prs a \linkS4class{PAMrSettings} object to remove a function from
#' @param index index indicating which function to move, counting from
#'   \code{ClickDetector} functions first, then \code{WhistlesMoans} functions.
#'   This is the same order functions appear in when examining the prs object.
#'   For example, if there are two Click functions and one Whistle function, the
#'   Whistle function would have an index of 3. If missing, user can select
#'   from a list.
#'
#' @return the same \linkS4class{PAMrSettings} object as prs, with the function
#'   removed from the "functions" slot
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom utils menu capture.output
#' @export
#'
removeFunction <- function(prs, index) {
    if(missing(index)) {
        index <- menu(title = 'Choose a function to remove:',
                      choices=unlist(sapply(prs@functions, function(x) {
                          paste(names(x), sapply(x, function(y) {
                              capture.output(str(y))
                          }))
                      })))
    }
    nFuns <- sapply(prs@functions, length)
    if(index > sum(nFuns)) warning('Index too large, no function to remove.')
    for(m in seq_along(prs@functions)) {
        if(index > nFuns[m]) {
            index <- index - nFuns[m]
            next
        }
        prs@functions[[m]] <- prs@functions[[m]][-index]
    }
    prs
}
