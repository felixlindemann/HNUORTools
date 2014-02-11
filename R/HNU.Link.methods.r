
## $
setMethod("$","HNULink",function(x,name) {return(slot(x,name))})

setMethod("$<-","HNULink",function(x,name,value) {
  slot(x,name,check=TRUE) <- value
  valid<-validObject(x)
  return(x)
}) 