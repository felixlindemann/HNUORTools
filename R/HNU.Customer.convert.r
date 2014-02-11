
#
#	by Felix Lindemann
#	konvertierungen
#
# 
as.HNUCustomer.list = function(x, ...){
	return(new("HNUCustomer", x))
}
as.HNUCustomer.data.frame = function(x, ...){
	return(new("HNUCustomer", x))
}
as.data.frame.HNUCustomer = function(x, ...){
	data.frame(id=x@id, label = x@label, x= x@x, y= x@y, demand = x@demand)
}
as.list.HNUCustomer = function(x, ...){
	list(id=x@id, label = x@label, x= x@x, y= x@y, demand = x@demand)
}
 
setGeneric("as.HNUCustomer", function(x, ...) standardGeneric( "as.HNUCustomer")) 
 
setMethod("as.HNUCustomer",     signature(x = "list"),  	  as.HNUCustomer.list) 
setMethod("as.HNUCustomer",  	signature(x = "data.frame"),  as.HNUCustomer.data.frame) 
 
setMethod("as.list",        signature(x = "HNUCustomer"),  	  as.list.HNUCustomer) 
setMethod("as.data.frame",  signature(x = "HNUCustomer"),  	  as.data.frame.HNUCustomer) 


setAs("data.frame", "HNUCustomer", def=function(from){
    return(as.HNUCustomer.data.frame(from))
})

setAs("list", "HNUCustomer", def=function(from){
    return(as.HNUCustomer.list(from))
})
 
 
#is.HNUCustomer
setGeneric("is.HNUCustomer",      function(x, ...) standardGeneric( "is.HNUCustomer")) 
setMethod( "is.HNUCustomer", "HNUCustomer", function(x, ...){return(is(x ,"HNUCustomer"))})
 