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
#'   a list a vector with length equal to the list. Can also be a dataframe
#'   with columns \code{event} and \code{species}, in which case species will
#'   be matched to corresponding event names instead of just relying on the
#'   order. If using this, please note the prefix OE or DGL present on most
#'   event numbers (see the \code{id} slot of your events).
#' @return the same object as \code{acev}, with species identifications assigned
#'   as an item named \code{type} in the \code{species} slot
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom dplyr distinct
#' @importFrom stringr str_trim str_split
#' @importFrom RSQLite dbConnect dbDisconnect dbReadTable SQLite
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
    method <- match.arg(method, choices = c('pamguard', 'manual', 'am'))
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
               if(inherits(value, 'data.frame')) {
                   if(!all(c('species', 'event') %in% colnames(value))) {
                       warning('If "value" is a dataframe it must contain columns species and event.')
                       return(acev)
                   }
                   for(i in seq_along(acev)) {
                       species(acev[[i]])[[type]] <- value[value$event == id(acev[[i]]), 'species']
                   }
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
           'am' = {
               specDf <- distinct(do.call(rbind, lapply(acev, function(oneAe) {
                   dbs <- files(oneAe)$database
                   events <- do.call(rbind, lapply(dbs, function(x) {
                       con <- dbConnect(x, drv=SQLite())
                       evs <- dbReadTable(con, 'Click_Detector_OfflineEvents')
                       dbDisconnect(con)
                       # browser()
                       evs <- evs[, c('UID', 'eventType', 'comment')]

                       evs$eventType <- str_trim(evs$eventType)
                       evs$comment <- gsub('OFF EFF', '', evs$comment)
                       evs$comment <- gsub("[[:punct:]]", '', evs$comment)
                       evs$comment <- str_trim(evs$comment)
                       evs
                   }))
                   events$event <- paste0('OE', as.character(events$UID))
                   events$species <- 'unid'
                   goodEvents <- c('BEAK', 'FORG')
                   events$species[events$eventType %in% goodEvents] <- str_split(events$comment[events$eventType %in% goodEvents],
                                                                         ' ', simplify=TRUE)[, 1]
                   events$species <- tolower(events$species)
                   events
               }
               )))
               cat('Assigning unique species: ', paste0(unique(specDf[specDf$event %in% sapply(acev, id), 'species']),
                                                       collapse = ', '), '.', sep = '')
               return(setSpecies(acev, method = 'manual', type=type, value = specDf))
           },
           warning('Method ', method, ' not supported.')
    )
    acev
}
