setClass('theFuck',
         slots = c(data = 'data.frame',
                   nrow = 'integer'),
         validity = function(object) {
           'x' %in% colnames(object@data)
         }
)
# this is what will show up in auto complete
setGeneric('theFuck',
           function(...) standardGeneric('theFuck'))

setMethod('theFuck', signature('data.frame'),
          function(data, ...) {
            new('theFuck', data=data, nrow=nrow(data), ...)
          })

setMethod('theFuck', signature('numeric'),
          function(vec, ...) {
            new('theFuck', data=data.frame(x=vec, y=vec), nrow=length(vec))
          })

setMethod('theFuck', signature('character'),
          function(char, ...) {
            new('theFuck', data=data.frame(x='hey', y=char), nrow=1L)
          })

### ACEV
clickdat <- data.frame(id=1:5, a=10:14, b=letters[1:5])
bpdat <- data.frame(id=1:10, c=21:30, a=letters[11:20], z=rep(1,10))
acev <- AcousticEvent(detectors=list(click=clickdat, bp=bpdat))
