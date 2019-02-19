#' @title Remove Binaries from a PAMrSettings Object
#'
#' @description Remove a binary folder and associated files from the "binaries"
#'   slot in a PAMrSettings object.
#'
#' @param prs a \linkS4class{PAMrSettings} object to remove binaries from
#' @param index index indicating which binary folders to remove. Can be a vector
#'   if you want to remove multiple folders. If missing user is prompted to
#'   select a folder from a list, will only show up to the first 20. You can
#'   easily remove all of the folders with a large index like \code{1:1000}
#'
#' @return the same \linkS4class{PAMrSettings} object as prs, with the binary
#'   folders and files associated with those folders removed from the "binaries"
#'   slot.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom utils menu
#' @export
#'
removeBinaries <- function(prs, index=NULL) {
    if(is.null(index)) {
        if(length(prs@binaries$folder) > 20) {
            warning('Only showing first 20 binary folders.')
            choices <- prs@binaries$folder[1:20]
        }
        choices <- prs@binaries$folder
        index <- menu(title = 'Choose a folder to remove:',
                      choices = choices)
    }
    if(max(index) > length(prs@binaries$folder)) warning('Index too large, no folder to remove.')
    dropNames <- prs@binaries$folder[index]
    for(f in dropNames) {
        if(is.na(f)) next
        prs@binaries$list <- prs@binaries$list[!grepl(f, prs@binaries$list, fixed=TRUE)]
    }
    prs@binaries$folder <- prs@binaries$folder[-index]
    prs
}
