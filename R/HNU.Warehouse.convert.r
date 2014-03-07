
#
#	by Felix Lindemann
#	konvertierungen
#
# 
as.Warehouse.list = function(x, ...){
	return(new("Warehouse", x))
}
as.Warehouse.data.frame = function(x, ...){
	return(new("Warehouse", x))
}
as.data.frame.Warehouse = function(x, ...){
	li<-list(...)
	if(is.null(li$withrownames)) li$withrownames <- FALSE
	df<-data.frame(id=x@id, label = x@label, x= x@x, y= x@y, fixcosts = x@fixcosts, supply = x@supply, open = x@open)
	if(li$withrownames) rownames(df)<-x@id
	return (df)
}
as.list.Warehouse = function(x, ...){
	list(id=x@id, label = x@label, x= x@x, y= x@y, fixcosts = x@fixcosts, supply = x@supply, open = x@open)
}
 
setGeneric("as.Warehouse", function(x, ...) standardGeneric( "as.Warehouse")) 
 
setMethod("as.Warehouse",     signature(x = "list"),  	  as.Warehouse.list) 
setMethod("as.Warehouse",  	signature(x = "data.frame"),  as.Warehouse.data.frame) 
 
setMethod("as.list",        signature(x = "Warehouse"),  	  as.list.Warehouse) 
setMethod("as.data.frame",  signature(x = "Warehouse"),  	  as.data.frame.Warehouse) 


setAs("data.frame", "Warehouse", def=function(from){
    return(as.Warehouse.data.frame(from))
})

setAs("list", "Warehouse", def=function(from){
    return(as.Warehouse.list(from))
})
 
 
#is.Warehouse
setGeneric("is.Warehouse",      function(x, ...) standardGeneric( "is.Warehouse")) 
setMethod( "is.Warehouse", "Warehouse", function(x, ...){return(is(x ,"Warehouse"))})
 