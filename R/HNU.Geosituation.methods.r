
setMethod("$","HNUGeoSituation",function(x,name) {return(slot(x,name))})

setMethod("$<-","HNUGeoSituation",function(x,name,value) {
  slot(x,name,check=TRUE) <- value
  valid<-validObject(x)
  return(x)
}) 


setGeneric("add",  function(object,value,...)  standardGeneric("add") )


setMethod("add",signature(object="HNUGeoSituation", value="HNUNode"),
	function(object,value,...){
		if(validObject(value) & is.HNUNode(value)){
			n<- length(object@nodes)+1
			object@nodes[[n]] <- value
		} 
	    return(object)
	}
)

setMethod("add",signature(object="HNUGeoSituation", value="HNUCustomer"),
	function(object,value,...){
		if(validObject(value) & is.HNUCustomer(value)){
			n<- length(object@customers)+1
			object@customers[[n]] <- value
		} 
	    return(object)
	}
)

setMethod("add",signature(object="HNUGeoSituation", value="HNUWarehouse"),
	function(object,value,...){
		if(validObject(value) & is.HNUWarehouse(value)){
			n<- length(object@warehouses)+1
			object@warehouses[[n]] <- value
		} 
	    return(object)
	}
)

setMethod("add",signature(object="HNUGeoSituation", value="HNULink"),
	function(object,value,...){
		if(validObject(value) & is.HNULink(value)){
			n<- length(object@links)+1
			object@links[[n]] <- value
		} 
	    return(object)
	}
)
