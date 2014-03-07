#
#	by Felix Lindemann
#	konvertierungen
#
# 
as.Link.list = function(x, ...){
	return(new("Link", x))
}
as.Link.data.frame = function(x, ...){
	return(new("Link", x))
}
as.data.frame.Link = function(x, ...){
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
as.list.Link = function(x, ...){
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
 
setGeneric("as.Link", function(x, ...) standardGeneric( "as.Link")) 
 
setMethod("as.Link",     signature(x = "list"),  	  as.Link.list) 
setMethod("as.Link",  	signature(x = "data.frame"),  as.Link.data.frame) 
 
setMethod("as.list",        signature(x = "Link"),  	  as.list.Link) 
setMethod("as.data.frame",  signature(x = "Link"),  	  as.data.frame.Link) 


setAs("data.frame", "Link", def=function(from){
    return(as.Link.data.frame(from))
})

setAs("list", "Link", def=function(from){
    return(as.Link.list(from))
})
 
 
#is.Link
setGeneric("is.Link",      function(x, ...) standardGeneric( "is.Link")) 
setMethod( "is.Link", "Link", function(x, ...){return(is(x ,"Link"))})
