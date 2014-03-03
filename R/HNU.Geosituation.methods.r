
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




setGeneric("getLink",  function(object,conn,...)  standardGeneric("getLink") )
setMethod( "getLink", signature(object= "HNUGeoSituation", conn = "character"),
  function(object, conn, ...){
    if(length(conn)!=2) stop("The Connection element has to have two elements exactly.")

    li <- sapply(object$links, function(o){if( (o$origin$id==conn[1] & o$destination$id ==conn[2]) | (o$origin$id==conn[2] & o$destination$id ==conn[1] & o$oneway==FALSE))return(o)})  
    li <- li[unlist(lapply(li, length) != 0)]
 
    return (li)

  }
) 

setGeneric("getNodesDataFrame",  function(object,...)  standardGeneric("getNodesDataFrame") )
setMethod( "getNodesDataFrame", signature(object= "HNUGeoSituation"),
  function(object,  ...){
    li <- list(...)
 
    df<-data.frame()

    for(i in 1:length(object$nodes)){
    	o <- object$nodes[[i]]
    	df <- rbind(df,as.data.frame(o,...))
    }
    return (df)
  }
)

setGeneric("getCustomersDataFrame",  function(object,...)  standardGeneric("getCustomersDataFrame") )
setMethod( "getCustomersDataFrame", signature(object= "HNUGeoSituation"),
  function(object,  ...){
    li <- list(...)
 
    df<-data.frame()

    for(i in 1:length(object$customers)){
    	o <- object$customers[[i]]
    	df <- rbind(df,as.data.frame(o,...))
    }
    return (df)
  }
)

setGeneric("getLinksDataFrame",  function(object,...)  standardGeneric("getLinksDataFrame") )
setMethod( "getLinksDataFrame", signature(object= "HNUGeoSituation"),
  function(object,  ...){
    li <- list(...)
 
    df<-data.frame()

    for(i in 1:length(object$links)){
    	o <- object$links[[i]] 
    	df <- rbind(df,as.data.frame(o,...))
    }
    return (df)
  }
)

setGeneric("getWarehousesDataFrame",  function(object,...)  standardGeneric("getWarehousesDataFrame") )
setMethod( "getWarehousesDataFrame", signature(object= "HNUGeoSituation"),
  function(object,  ...){
    li <- list(...)
 
    df<-data.frame()

    for(i in 1:length(object$warehouses)){
    	o <- object$warehouses[[i]]
    	df <- rbind(df,as.data.frame(o,...))
    }
    return (df)
  }
)


