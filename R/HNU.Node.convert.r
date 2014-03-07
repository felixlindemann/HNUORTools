#
#	by Felix Lindemann
#	konvertierungen
#
# 
as.Node.list = function(x, ...){
	return(new("Node", x))
}
as.Node.data.frame = function(x, ...){
	return(new("Node", x))
}
as.data.frame.Node = function(x, ...){
	li<-list(...)
	if(is.null(li$withrownames)) li$withrownames <- FALSE
	df<-data.frame(id=x@id, label = x@label, x= x@x, y= x@y)
	if(li$withrownames) rownames(df)<-x@id
	return (df)
}
as.list.Node = function(x, ...){
	list(id=x@id, label = x@label, x= x@x, y= x@y)
}
 
setGeneric("as.Node", function(x, ...) standardGeneric( "as.Node")) 
 
setMethod("as.Node",     signature(x = "list"),  	  as.Node.list) 
setMethod("as.Node",  	signature(x = "data.frame"),  as.Node.data.frame) 
 
setMethod("as.list",        signature(x = "Node"),  	  as.list.Node) 
setMethod("as.data.frame",  signature(x = "Node"),  	  as.data.frame.Node) 


setAs("data.frame", "Node", def=function(from){
    return(as.Node.data.frame(from))
})

setAs("list", "Node", def=function(from){
    return(as.Node.list(from))
})
 
 
#is.Node
setGeneric("is.Node",      function(x, ...) standardGeneric( "is.Node")) 
setMethod( "is.Node", "Node", function(x, ...){return(is(x ,"Node"))})
