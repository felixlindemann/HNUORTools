 #' @title Extract Method
#' @name $
#' @aliases $,GeoSituation-method 
#' @rdname extract-methods 
setMethod("$","GeoSituation",function(x,name) {return(slot(x,name))})
 #' @title Set Method 
#' @name $<-
#' @aliases $<-,GeoSituation-method 
#' @rdname set-methods
setMethod("$<-","GeoSituation",function(x,name,value) {
  slot(x,name,check=TRUE) <- value
  valid<-validObject(x)
  return(x)
}) 

##############################################################
#' @name add 
#' @docType methods
#' @export  
#' @rdname GeoSituation-Add-method
#'
#' @title Add an Object to a \code{\link{GeoSituation}}
#' @description This method should simplify the use of HNUORTools.
#'  
#' @param object an object of class \code{\link{GeoSituation}}
#' @param value an object of class \code{\link{Node}}, \code{\link{Link}}, \code{\link{Customer}} or \code{\link{Warehouse}}
#' @param ... Additional argument list that might not ever be used.
#' @return The updated object of class \code{\link{GeoSituation}}.
#' @note 
#'      for citing use: Felix Lindemann (2014). HNUORTools: Operations Research Tools. R package version 1.1-0. \url{http://felixlindemann.github.io/HNUORTools/}.
#'      
#' @author Dipl. Kfm. Felix Lindemann \email{felix.lindemann@@hs-neu-ulm.de} 
#' 
#' Wissenschaftlicher Mitarbeiter
#' Kompetenzzentrum Logistik
#' Buro ZWEI, 17
#'
#' Hochschule fur angewandte Wissenschaften 
#' Fachhochschule Neu-Ulm | Neu-Ulm University 
#' Wileystr. 1 
#' 
#' D-89231 Neu-Ulm 
#' 
#' 
#' Phone   +49(0)731-9762-1437 
#' Web      \url{www.hs-neu-ulm.de/felix-lindemann/} 
#'      \url{http://felixlindemann.blogspot.de}
setGeneric("add",  function(object,value,...)  standardGeneric("add") )


#' @aliases add,GeoSituation,Node-method
#' @rdname GeoSituation-Add-method
setMethod("add",signature(object="GeoSituation", value="Node"),
	function(object,value,...){
		if(validObject(value) & is.Node(value)){
			n<- length(object@nodes)+1
			object@nodes[[n]] <- value
		} 
	    return(object)
	}
)

#' @aliases add,GeoSituation,Customer-method
#' @rdname GeoSituation-Add-method
setMethod("add",signature(object="GeoSituation", value="Customer"),
	function(object,value,...){
		if(validObject(value) & is.Customer(value)){
			n<- length(object@customers)+1
			object@customers[[n]] <- value
		} 
	    return(object)
	}
)

#' @aliases add,GeoSituation,Warehouse-method
#' @rdname GeoSituation-Add-method
setMethod("add",signature(object="GeoSituation", value="Warehouse"),
	function(object,value,...){
		if(validObject(value) & is.Warehouse(value)){
			n<- length(object@warehouses)+1
			object@warehouses[[n]] <- value
		} 
	    return(object)
	}
)

#' @aliases add,GeoSituation,Link-method
#' @rdname GeoSituation-Add-method
setMethod("add",signature(object="GeoSituation", value="Link"),
	function(object,value,...){
		if(validObject(value) & is.Link(value)){
			n<- length(object@links)+1
			object@links[[n]] <- value
		} 
	    return(object)
	}
)




setGeneric("getLink",  function(object,conn,...)  standardGeneric("getLink") )
setMethod( "getLink", signature(object= "GeoSituation", conn = "character"),
  function(object, conn, ...){
    if(length(conn)!=2) stop("The Connection element has to have two elements exactly.")

    li <- sapply(object$links, function(o){if( (o$origin$id==conn[1] & o$destination$id ==conn[2]) | (o$origin$id==conn[2] & o$destination$id ==conn[1] & o$oneway==FALSE))return(o)})  
    li <- li[unlist(lapply(li, length) != 0)]
 
    return (li)

  }
) 

setGeneric("getNodesDataFrame",  function(object,...)  standardGeneric("getNodesDataFrame") )
setMethod( "getNodesDataFrame", signature(object= "GeoSituation"),
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
setMethod( "getCustomersDataFrame", signature(object= "GeoSituation"),
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
setMethod( "getLinksDataFrame", signature(object= "GeoSituation"),
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
setMethod( "getWarehousesDataFrame", signature(object= "GeoSituation"),
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


