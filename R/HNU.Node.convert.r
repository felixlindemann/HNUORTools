#
#	by Felix Lindemann
#	konvertierungen
#
# 
as.HNUNode.list = function(x, ...){
	return(new("HNUNode", x))
}
as.HNUNode.data.frame = function(x, ...){
	return(new("HNUNode", x))
}
as.data.frame.HNUNode = function(x, ...){
	data.frame(id=x@id, label = x@label, x= x@x, y= x@y)
}
as.list.HNUNode = function(x, ...){
	list(id=x@id, label = x@label, x= x@x, y= x@y)
}
 
setGeneric("as.HNUNode", function(x, ...) standardGeneric( "as.HNUNode")) 
 
setMethod("as.HNUNode",     signature(x = "list"),  	  as.HNUNode.list) 
setMethod("as.HNUNode",  	signature(x = "data.frame"),  as.HNUNode.data.frame) 
 
setMethod("as.list",        signature(x = "HNUNode"),  	  as.list.HNUNode) 
setMethod("as.data.frame",  signature(x = "HNUNode"),  	  as.data.frame.HNUNode) 


setAs("data.frame", "HNUNode", def=function(from){
    return(as.HNUNode.data.frame(from))
})

setAs("list", "HNUNode", def=function(from){
    return(as.HNUNode.list(from))
})
 
 
#is.HNUNode
setGeneric("is.HNUNode",      function(x, ...) standardGeneric( "is.HNUNode")) 
setMethod( "is.HNUNode", "HNUNode", function(x, ...){return(is(x ,"HNUNode"))})
