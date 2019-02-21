#' @title Remove a Function from a PAMrSettings Object
#'
#' @description Remove a function from the "function" slot in a PAMrSettings
#'   object.
#'
#' @param prs a \linkS4class{PAMrSettings} object to remove a function from
#' @param index index indicating which function to move, counting from
#'   \code{ClickDetector} functions first, then \code{WhistlesMoans} functions,
#'   then \code{Cepstrum} functions.
#'   This is the same order functions appear in when examining the prs object.
#'   For example, if there are two Click functions and one Whistle function, the
#'   Whistle function would have an index of 3. If missing, user can select
#'   from a list. This can also be a vector to remove multiple functions at once.
#'
#' @return the same \linkS4class{PAMrSettings} object as prs, with the function
#'   removed from the "functions" slot
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom utils menu capture.output
#' @export
#'
removeFunction <- function(prs, index=NULL) {
    if(is.null(index)) {
        index <- menu(title = 'Choose a function to remove:',
                      choices=unlist(sapply(prs@functions, function(x) {
                          paste(names(x), sapply(x, function(y) {
                              capture.output(str(y))[[1]]
                          }))
                      })))
        if(index==0) return(prs)
    }
    index <- sort(index) # needed if vector for recursion to work
    nFuns <- sapply(prs@functions, length)
    if(index[1] > sum(nFuns)) warning('Index too large, no function to remove.')
    for(m in seq_along(prs@functions)) {
        if(index[1] > nFuns[m]) {
            index[1] <- index[1] - nFuns[m]
            next
        }
        prs@functions[[m]] <- prs@functions[[m]][-index[1]]
        break
    }
    # allows for recursive removal of index as a vector. -1 cuz we dropped one function
    if(length(index) > 1) {
        return(removeFunction(prs, index[-1]-1))
    }
    prs
}
