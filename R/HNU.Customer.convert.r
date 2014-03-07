
#
#	by Felix Lindemann
#	konvertierungen
#
# 
as.Customer.list = function(x, ...){
	return(new("Customer", x))
}
as.Customer.data.frame = function(x, ...){
	return(new("Customer", x))
}
as.data.frame.Customer = function(x, ...){
	li<-list(...)
	if(is.null(li$withrownames)) li$withrownames <- FALSE
	df<-	data.frame(id=x@id, label = x@label, x= x@x, y= x@y, demand = x@demand)
	if(li$withrownames) rownames(df)<-x@id
	return (df)
}
as.list.Customer = function(x, ...){
	list(id=x@id, label = x@label, x= x@x, y= x@y, demand = x@demand)
}
 
setGeneric("as.Customer", function(x, ...) standardGeneric( "as.Customer")) 
 
setMethod("as.Customer",     signature(x = "list"),  	  as.Customer.list) 
setMethod("as.Customer",  	signature(x = "data.frame"),  as.Customer.data.frame) 
 
setMethod("as.list",        signature(x = "Customer"),  	  as.list.Customer) 
setMethod("as.data.frame",  signature(x = "Customer"),  	  as.data.frame.Customer) 


setAs("data.frame", "Customer", def=function(from){
    return(as.Customer.data.frame(from))
})

setAs("list", "Customer", def=function(from){
    return(as.Customer.list(from))
})
 
 
#is.Customer
setGeneric("is.Customer",      function(x, ...) standardGeneric( "is.Customer")) 
setMethod( "is.Customer", "Customer", function(x, ...){return(is(x ,"Customer"))})
 