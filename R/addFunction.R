#' @title Add a Function to a PAMrSettings Object
#'
#' @description Adds a new function to the "function" slot in a PAMrSettings
#'   object.
#'
#' @param prs a \linkS4class{PAMrSettings} object to add a function to
#' @param fun function to add OR another \linkS4class{PAMrSettings} object.
#'   In this case all functions from the second object will be added to \code{prs}
#' @param module PamGuard module output this function should act on
#'
#' @return the same \linkS4class{PAMrSettings} object as prs, with the function
#'   \code{fun} added to the "functions" slot
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom utils menu
#' @export
#'
addFunction <- function(prs, fun, module=NULL) {
    modsAllowed <- c('ClickDetector', 'WhistlesMoans', 'Cepstrum')
    if(class(fun) == 'PAMrSettings') {
        for(m in modsAllowed) {
            newFuns <- fun@functions[[m]]
            cat('Adding', length(newFuns), 'functions to module type', m, '\n')
            prs@functions[m] <- list(c(prs@functions[[m]], newFuns))
        }
        return(prs)
    }
    fname <- deparse(substitute(fun))
    if(is.null(module) ||
       !(module %in% modsAllowed)) {
        chooseMod <- menu(choices = modsAllowed,
                          title = c('What module type is this function for?'))
        if(chooseMod==0) stop('You must set a module type for this function.')
        module <- modsAllowed[chooseMod]
    }
    cat('Adding function "', fname, '":\n', sep = '')
    oldnames <- names(prs@functions[[module]])
    fun <- functionParser(fun)
    # function checker
    if(functionChecker(fun, module)) {
        prs@functions[module] <- list(c(prs@functions[[module]], fun))
        names(prs@functions[[module]]) <- c(oldnames, fname)
    } else {
        cat('Unable to add function ', fname, ', it did not create the expected output.')
    }
    prs
}

# I put a function in yo function cuz i heard you like functions
functionParser <- function(fun) {
    argList <- formals(fun)
    toSet <- names(argList)[!(names(argList) %in% c('data', 'calibration', '...'))]
    if(length(toSet) > 0) {
        for(a in toSet) {
            cat('Set a value for parameter "', a, '", please put quotes around strings', sep='')
            if(class(argList[[a]]) == 'name') {
                cat(' (no default value found):')
            } else {
                cat(' (default value is ', argList[[a]], '):', sep = '')
            }
            val <- readline()
            # If it evals properly, do that. Otherwise its prob a string so leave it
            newVal <- tryCatch({
                # empty so you dont accidentally grab vars, or do you want this????
                # just change to globalenv() if you want that
                eval(parse(text=val), envir = globalenv())
            },
            error = function(e) {
                val
            })
            if(is.null(newVal)) next
            argList[a] <- newVal
        }
        formals(fun) <- argList
    }
    fun
}

functionChecker <- function(fun, module) {
    switch(module,
           'ClickDetector' = clickChecker(fun),
           'WhistlesMoans' = whistleChecker(fun),
           'Cepstrum' = cepstrumChecker(fun),
           FALSE
    )
}

clickChecker <- function(fun) {
    good <- TRUE
    tryCatch({
        testThisClick <- fun(data=PAMr::testClick)
    },
    error = function(e) {
        print(e)
        good <<- FALSE
    })
    if(!exists('testThisClick')) {
        cat('Click function did not run succesfully.')
        return(FALSE)
    }
    if(is.null(testThisClick)) {
        cat('Click function returned nothing.')
        return(FALSE)
    }
    if(nrow(testThisClick) != 2) {
        cat('Click functions should return 1 row for each channel.')
        good <- FALSE
    }
    good
}

whistleChecker <- function(fun) {
    TRUE
}

cepstrumChecker <- function(fun) {
    TRUE
}
