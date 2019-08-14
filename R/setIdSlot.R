#' @title Temp Function to Assign Id Slot
#'
#' @description Uses the names of a list to set the Id slot of AcousticEvent
#'   objects, only used to update to v0.6.0, not needed after
#'
#' @param evList a named list of \linkS4class{AcousticEvent} objects
#'
#' @return the same list of \linkS4class{AcousticEvent} objects, but with the
#'   id slot added and set to the names of the list
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @export
#'
setIdSlot <- function(evList) {
    if(is.null(names(evList))) {
        stop('List must be named.')
    }
    for(i in seq_along(evList)) {
        evList[[i]]@id <- names(evList)[i]
    }
    evList
}
