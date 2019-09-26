#' @title Set the Species Classification of an Acoustic Event
#'
#' @description Sets the \code{species} slot of an \linkS4class{AcousticEvent}
#'   object or list of objects
#'
#' @param acev a \linkS4class{AcousticEvent} object, or list of objects
#' @param type the type of classification to set, this is just a label within
#'   the \code{species} slot
#' @param method the method for assigning species to an event. Currently supports
#'   \code{pamguard}, which will use the 'eventType' or 'Text_Annotation' column
#'   to assign species, or \code{manual} which will use \code{value} to assign
#'   species manually.
#' @param value optional argument required if \code{method} is set to manual.
#'   Can either be a single value to assign to all events, or if assigning to
#'   a list a vector with length equal to the list.
#' @return the same object as \code{acev}, with species identifications assigned
#'   as an item named \code{type} in the \code{species} slot
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @export
#'
setSpecies <- function(acev, type='id', method=c('pamguard', 'manual'), value) {
    # jank for now, using names to report shit
    if('AcousticEvent' %in% class(acev)) {
        tempName <- basename(files(acev)$database[1])
        tempName <- gsub('\\..*$',  '', tempName)
        acev <- list(acev)
        names(acev) <- tempName
    }
    method <- match.arg(method)
    switch(method,
           'pamguard' = {
               spCol <- c('Text_Annotation', 'eventType', 'eventLabel')
               for(i in seq_along(acev)) {
                   sp <- sapply(detectors(acev[[i]]), function(x) {
                       hasCol <- spCol[spCol %in% colnames(x)]
                       unique(x[, hasCol])
                   })
                   sp <- unique(sp)
                   if(length(sp) > 1) {
                       spix <- menu(title = paste0('More than one species found for event ',
                                                   names(acev)[i], ', select one to assign:'),
                                    choices = sp)
                       if(spix == 0) return(acev) # or just NA and next?
                       sp <- sp[spix]
                   }
                   species(acev[[i]])[[type]] <- sp
               }
           },
           'manual' = {
               if(missing(value)) {
                   warning('Manual mode requires a "value" to set."')
                   return(acev)
               }
               if(length(value) != 1 &&
                  length(value) != length(acev)) {
                   warning('Length of "value" must be either 1 or the number of events.')
                   return(acev)
               }
               if(length(value) == 1) {
                   value <- rep(value, length(acev))
               }
               for(i in seq_along(acev)) {
                   species(acev[[i]])[[type]] <- value[i]
               }
           },
           warning('Method ', method, ' not supported.')
    )
    acev
}
