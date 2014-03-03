#
#	by Felix Lindemann
#	konvertierungen
#
# 
as.HNULink.list = function(x, ...){
	return(new("HNULink", x))
}
as.HNULink.data.frame = function(x, ...){
	return(new("HNULink", x))
}
as.data.frame.HNULink = function(x, ...){
	li<-list(...)
	if(is.null(li$withrownames)) li$withrownames <- FALSE
	df<-data.frame(
		id=x@id, label = x@label, 
		origin= x@origin$id, 
		destination = x@destination$id, 
		costs = x@costs,
        distance = x@distance,
        oneway = x@oneway,
        used = x@used)
	if(li$withrownames)	rownames(df)<-x@id
	return (df)
}
as.list.HNULink = function(x, ...){
	li<-list(
		id=x@id, label = x@label, 
		origin= x@origin$id, 
		destination = x@destination$id, 
		costs = x@costs,
        distance = x@distance,
        oneway = x@oneway,
        used = x@used)
	return(li)
}
 
setGeneric("as.HNULink", function(x, ...) standardGeneric( "as.HNULink")) 
 
setMethod("as.HNULink",     signature(x = "list"),  	  as.HNULink.list) 
setMethod("as.HNULink",  	signature(x = "data.frame"),  as.HNULink.data.frame) 
 
setMethod("as.list",        signature(x = "HNULink"),  	  as.list.HNULink) 
setMethod("as.data.frame",  signature(x = "HNULink"),  	  as.data.frame.HNULink) 


setAs("data.frame", "HNULink", def=function(from){
    return(as.HNULink.data.frame(from))
})

setAs("list", "HNULink", def=function(from){
    return(as.HNULink.list(from))
})
 
 
#is.HNULink
setGeneric("is.HNULink",      function(x, ...) standardGeneric( "is.HNULink")) 
setMethod( "is.HNULink", "HNULink", function(x, ...){return(is(x ,"HNULink"))})
