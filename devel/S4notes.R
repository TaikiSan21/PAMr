# Eric's S3/S4 class 02/06/2018
x <- rnorm(300)
y <- rnorm(300)

z <- factor(sample(letters, 300, replace=TRUE))

summary(z)
class(z) <- 'myClass'
summary(z)

# . means define summary for myClass
summary.myClass <- function(x) {
  length(x)
}
summary(z)

######
df <- iris
class(df) <- 'irisData'
plot.irisData <- function(x) {
  plot(x$Sepal.Length, x$Sepal.Width)
}
plot(df)
summary(df) # doesn't work
df <- iris
class(df) <- c('irisData', 'data.frame')
plot(df)
summary(df) # works. saw irisData, then data.frame

###### BASIC S3 STRUCTURES AND METHODS ########
df <- list(
  data=iris, date=Sys.Date()
)
class(df) <- c('irisData', class(df))
summary.irisData <- function(x) summary(x$data)
summary(df)
# defining a print is what it does when you just hit df ENTER in console
print.irisData <- function(x) print(summary(x))

# if we want a new method: 'date' is not a genereic
methods('date') # none yet
date <- function(x) UseMethod('date') # Do this for creating a generic, then
date.irisData <- function(x) print(x$date) # specifics
date.default <- function(x) cat('lolno') #defaults

##### DEFINING FOR EXISTING NON-GENERIC METHODS
# if we want an nrow function for this shit. nrow is not a generic
nrow.irisData <- function(x) nrow(x$data)
nrow(df) # doesnt work
nrow <- function(x) UseMethod('nrow') # need to make it a generic, then
nrow.default <- function(x) base::nrow(x) # nrow happens to be in base. have to do this or everything breaks
nrow.irisData <- function(x) nrow(x$data)
nrow(df)

### CONSTRUCTORS ###
### Create a constructor function that makes shit with appropriate info. usually name of class
irisData <- function(data, date=Sys.Date()) {
  x <- list(data=data,
            date=date
  )
  class(x) <- c('irisData', class(x))
  x
}
iris1 <- irisData(1)
class(iris1)
date(iris1)
summary(iris1)
# Can do constructors as generics to handle different cases
irisData <- function(data, date) UseMethod('irisData')
# This will be our default - we always want data to be a df. So no default, force it to be a df
irisData.data.frame <- function(data, date=Sys.Date()) {
  x <- list(data=data,
            date=date
  )
  class(x) <- c('irisData', class(x))
  x
}
# Instead of writing conditionals to check what first input is, just make separate methods
# Cleaner than chaining ifs in our 'standard' builder. Here were assuming a character means
# a file path
irisData.character <- function(filename, date=Sys.Date()) {
  data <- read.csv(filename)
  irisData(data, date)
}

### ACCESSORS ###
date <- function(x) UseMethod('date')
# This can both get the date and assign the date depending on whats in value
date.irisData <- function(x, value=NULL) {
  if(is.null(value)) {
    x$date
  } else {
    x$date <- value
    x
  }
}
date(df)
df <- date(df, '2018-11-21') # doesnt work because our generic only takes 1 argument
date <- function(x, ...) UseMethod('date')
date(df)
df <- date(df, '2018-11-21') # Now works. Having (...) is a good habit
date(df)

# Pretty janky to need to reassign like this. Better:
date.irisData <- function(x) x$date
'date<-' <- function(x, ...) UseMethod('date<-')
'date<-.irisData' <- function(x, value) {
  x$date <- value
  x
}
'date<-.default' <- function(x, value) cat('lolsrs')
date(df)
date(df) <- 'today'
date(df)

# Also works for [
'[.irisData' <- function(x, i, j) {
  x$data[i, j]
}
df[1,2]
'[<-.irisData' <- function(x, i, j, value) {
  x$data[i, j] <- value
  x
}
df[1,2] <- 3
df[1,2]
methods(class='irisData')

### CONVERTERS
# 'as' is not a generic, but 'as.list', 'as.data.frame' are
as.data.frame.irisData <- function(x) x$data
as.data.frame(df) %>% head()

####### END S3 METHODS AND STRUCTURES #####
####### COMMENCE THE ESS FOUR ######
# S4 enforces/defines rigorous structure
# S3 basically just assumes it works (walks like a duck...)

# Creating a class. Now R knows about it

setClass(
  Class = 'irisS4',
  slots = c(
    data = 'data.frame',
    date = 'POSIXct',
    name = 'character'
  )
)

i4 <- new('irisS4', data=iris, date=Sys.time(), name='Snuggles') # how we doooo it
i4
i4@date # S4s get shit with @

# CONSTRUCTOR AGAIN
irisS4 <- function(data, name, date=Sys.time()) {
  new('irisS4', data=data, name=name, date=date)
}
ifour <- irisS4(iris, 'Wuggles')
ifour@name

# Can also do this generic way. Mobetta? (constructor)
irisS4 <- function(data, ...) UseMethod('irisS4')
irisS4.data.frame <- function(data, name, date=Sys.time()) {
  new('irisS4', data=data, name=name, date=date)
}
irisS4.character <- function(filename, name, date=Sys.time()) {
  data <- read.csv(filename)
  irisS4(data, name, date)
}

plot.irisS4 <- function(x) plot(x@data) # This doesn't work for S4s.
# Diff than above because here 'plot' is function, before 'irisS4' was func

setMethod(
  f = 'plot',  # generic to extend
  signature = 'irisS4',  # class to do it
  definition = function(x) { # what it do shorty
    plot(x@data)
  }
)
plot(ifour)

# S4s are printed to console using 'show' not 'print'
setMethod(
  f = 'show',
  signature ='irisS4',
  definition = function(object) { # the input here has to be named same as function. show calls for 'object'
    cat('name is ', object@name, '\n')
    print(head(object@data))
  }
)
ifour
# Setting generic shiiiit
setGeneric(
  name = 'nrow',
  def = function(x, ...) standardGeneric('nrow')
)
setMethod('nrow', 'irisS4', function(x) base::nrow(x@data))
setMethod('nrow', 'default', function(x) base::nrow(x)) # if we forget this then regular nrow breaks b/c 194-7
nrow(ifour)

# Checking shit is the way it be

setValidity(
  Class = 'irisS4',
  method = function(object) {
    goodCols <- setequal(
      colnames(object@data),
      c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width",  "Species")
    )
    goodDate <- object@date < Sys.time()
    if(goodCols & goodDate) {
      TRUE
    } else {
      'Yooooo. You suck' # If it doesnt return true, will return this
    }
  }
)
irisS4(iris, name='Wibfle', date=Sys.time()+1) # err cuz time is bad
irisS4(iris, name='nomnom', date=Sys.time()-1)
# Can just check. Good to check inputs in functions. People can change data that will
# break your validity checks, and you won't know unless you check. Good to put this into
# any accessors that let people muck about
validObject(irisS4(iris, name='Wibfle', date=Sys.time()-1))

##### INHERITANCE
# Did it weakly in S3 with class('irisDat', 'data.frame') by having multiple in there

validObject(irisS4(iris, name='Wibfle', date=Sys.time()+1))

setClass(
  'eatsIris',
  slots = c(
    tummy = 'character'
  ),
  contains = 'irisS4'
)

eats <- new('eatsIris', name='halsb', tummy='nom', date=Sys.time(), data=iris)
eats@tummy
nrow(eats)
