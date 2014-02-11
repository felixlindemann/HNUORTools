
#
#	by Felix Lindemann
#	konvertierungen
#
# 
as.HNUWarehouse.list = function(x, ...){
	return(new("HNUWarehouse", x))
}
as.HNUWarehouse.data.frame = function(x, ...){
	return(new("HNUWarehouse", x))
}
as.data.frame.HNUWarehouse = function(x, ...){
	data.frame(id=x@id, label = x@label, x= x@x, y= x@y, fixcosts = x@fixcosts, supply = x@supply, open = x@open)
}
as.list.HNUWarehouse = function(x, ...){
	list(id=x@id, label = x@label, x= x@x, y= x@y, fixcosts = x@fixcosts, supply = x@supply, open = x@open)
}
 
setGeneric("as.HNUWarehouse", function(x, ...) standardGeneric( "as.HNUWarehouse")) 
 
setMethod("as.HNUWarehouse",     signature(x = "list"),  	  as.HNUWarehouse.list) 
setMethod("as.HNUWarehouse",  	signature(x = "data.frame"),  as.HNUWarehouse.data.frame) 
 
setMethod("as.list",        signature(x = "HNUWarehouse"),  	  as.list.HNUWarehouse) 
setMethod("as.data.frame",  signature(x = "HNUWarehouse"),  	  as.data.frame.HNUWarehouse) 


setAs("data.frame", "HNUWarehouse", def=function(from){
    return(as.HNUWarehouse.data.frame(from))
})

setAs("list", "HNUWarehouse", def=function(from){
    return(as.HNUWarehouse.list(from))
})
 
 
#is.HNUWarehouse
setGeneric("is.HNUWarehouse",      function(x, ...) standardGeneric( "is.HNUWarehouse")) 
setMethod( "is.HNUWarehouse", "HNUWarehouse", function(x, ...){return(is(x ,"HNUWarehouse"))})
 