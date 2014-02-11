
## $
setMethod("$","HNUNode",function(x,name) {return(slot(x,name))})

setMethod("$<-","HNUNode",function(x,name,value) {
  slot(x,name,check=TRUE) <- value
  valid<-validObject(x)
  return(x)
})


## give attribute(slot) names
setMethod("names",   signature(x = "HNUNode"), function(x){ return(slotNames(x))}) 
 
# Basic Methods
setGeneric("calc.Distance", function(n1,n2, costs = 1, ...) standardGeneric("calc.Distance"))
setMethod( "calc.Distance", signature(n1 = "HNUNode", n2 = "HNUNode"),
    function(n1,n2, costs = 1, ...) {
         
        if(!is.HNUNode(n1)) stop("Node 1 is not of type HNUNode")
        if(!is.HNUNode(n2)) stop("Node 2 is not of type HNUNode")
        validObject(n1)
        validObject(n2)
         
        dist <- sqrt( (n1@x - n2@x)^2 + (n1@y - n2@y)^2 ) * costs
        return(dist)
    }
)