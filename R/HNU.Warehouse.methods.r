
## $
setMethod("$","HNUWarehouse",function(x,name) {return(slot(x,name))})

setMethod("$<-","HNUWarehouse",function(x,name,value) {
  slot(x,name,check=TRUE) <- value
  valid<-validObject(x)
  return(x)
}) 