#' @exportClass Node
#' @name Node 
#' @rdname Node
#' @aliases Node-class 
#' @title The Node class
#'
#' @description 
#' This class is part of the \pkg{HNUORTools}. It represents 
#' the base class for every locateable class in an
#' \dfn{Operations-Research (OR)}-context.
#' 
#' @details Find here the defined slots for this class.
#'      @section Slots: 
#'          \describe{
#'              \item{\code{id}:}{
#'                  Object of class \code{"character"}, containing data from id.
#'                  \strong{Should be unique}.
#'                  The default value will be caluclated randomly.
#'              }
#'              \item{\code{label}:}{
#'                  Object of class \code{"character"}, containing the label of 
#'                  the \code{\link{Node}}.
#'                  The default value will be caluclated randomly.
#'              }
#'              \item{\code{x}:}{
#'                  Object of class \code{"numeric"}, containing the x-coordinate 
#'                  of the \code{\link{Node}}.
#'                  The default value will be caluclated randomly.
#'              }
#'              \item{\code{y}:}{
#'                  Object of class \code{"numeric"}, containing the y-coordinate 
#'                  of the \code{\link{Node}}.
#'                  The default value will be caluclated randomly.
#'              }
#'          }
#'
#' @seealso The classes are derived from this class and the following Methods can be used with this class.:
#'      @section Creating objects of type \code{\link{Node}}:
#'          \describe{
#'              \item{Creating an \code{S4-Object}}{ 
#'                  \code{new("Node", ...)}
#'              } 
#'              \item{Converting from a \code{data.frame}}{ 
#'                  \code{as.Node{<data.frame>}}
#'                  See also below in the Methods-Section.
#'              }
#'              \item{Converting from a \code{list}}{ 
#'                  \code{as.Node{<list>}}
#'                  See also below in the Methods-Section.
#'              }
#'          }
#'      @section Methods:
#'          \describe{
#'              \item{\code{as.list(node, ...)}}{
#'                  Converts a \code{\link{Node}} into a \code{\link{list}}.
#'                  \code{...} are user-defined (not used) parameters.
#'              }
#'              \item{\code{as.data.frame(node, ...)}}{
#'                  Converts a \code{\link{Node}} into a \code{\link{data.frame}}.
#'                  \code{as.data.frame} accepts the optional parameter \code{withrownames} of class \code{"logical"} (default is \code{TRUE}). 
#'                  If \code{withrownames == TRUE} the returned \code{\link{data.frame}} will recieve the \code{id} as rowname.
#'                  \code{...} are user-defined (not used) parameters.
#'
#'              }
#'              \item{\code{as.Node(obj)}}{
#'                  Converts an object of class \code{\link{data.frame}} or of class \code{\link{list}} into a \code{\link{Node}}.
#'              } 
#'              \item{\code{is.Node(obj)}}{
#'                  Checks if the object \code{obj} is of type \code{\link{Node}}.
#'              } 
#'              \item{\code{\link{getDistance}}}{
#'                  Calculating the distance between two \code{\link{Node}s}.
#'              }
#'              \item{\code{\link{getpolar}}}{
#'                  Calculating the angle to the x-Axis of a link, 
#'                  connecting two \code{\link{Node}s}.
#'              }
#'          } 
#'   
#'      @section Derived Classes:
#'          \describe{
#'              \item{\code{\link{Customer}}}{
#'                  This class extends the \code{\link{Node}}-Class 
#'                  with attributes for according to customers for OR-Problems.
#'              }
#'              \item{\code{\link{Warehouse}}}{
#'                  This class extends the \code{\link{Node}}-Class 
#'                  with attributes for according to warehouses for OR-Problems.
#'              }
#'              \item{\code{\link{Link}}}{
#'                  This class is constructed by two entities of the \code{\link{Node}}-Class 
#'                  representing the origin and destination of \code{\link{Link}} for OR-Problems.
#'              }
#'          }
#'      @section To be used for:
#'          \describe{
#'              \item{Travelling-Salesman-Problem}{
#'                  Finding the shortest roundtrip with e.g.
#'                  Nearest-Neighbor-Method (\code{\link{TSP.NearestNeighbor}}),
#'                  Two-Opt (\code{\link{TSP.2OPT}}), 
#'                  Three-Opt (\code{\link{TSP.3OPT}})
#'              }
#'              \item{Shortest-Path-Problem}{
#'                  Calculating the shortest Path in a network of 
#'                  \code{\link{Link}}s using e.g. 
#'                  the algorithm of Dijkstra (\code{\link{SPP.Dijkstra}}), 
#'              }
#'          } 
#'    
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
#'          \url{http://felixlindemann.blogspot.de}
#' @examples 
#' # demo(HNUNode01)
#' # demo(HNUNode02)
#' # demo(HNUNode03) 
setClass(
	Class = "Node",
    representation=representation(
    	id     = "character",
        label  = "character",
    	x      = "numeric",
    	y      = "numeric"
    ),
    prototype=prototype(
    	list(
    		id    = character(),
            label = character(),
            x     = numeric(),
            y     = numeric() 
    	)
    ),
    validity = function(object){
        if( sum(is.null(object@x)) + sum( is.na(object@x)) > 0 ) {
            return(paste("Error with value x: Value is not initialized", class(object@x)))
        } else if( sum(is.null(object@y)) + sum( is.na(object@y)) > 0) {
            return(paste("Error with value y: Value is not initialized", class(object@y)))
        } else if( class(object@x)!="numeric" ) {
            return(paste("Error with value x: expected numeric datatype, but obtained", class(object@x)))
        } else if( class(object@y)!="numeric" ) {
            return(paste("Error with value y: expected numeric datatype, but obtained", class(object@y)))
        } else if( length(object@x)!=1 ){ 
            return(paste("Error with value x: expected numeric data of length 1, but obtained", length(object@x), ": no arrays supported here"))
        } else if( length(object@y)!=1 ){ 
            return(paste("Error with value y: expected numeric data of length 1, but obtained", length(object@y), ": no arrays supported here"))
        } else{ 
            return(TRUE)
        }
    } 
)
################################### initialize - Method ###################################################
#' @title initialize Method
#' @name initialize
#' @aliases initialize,Node-method 
#' @rdname initialize-methods 
#' @param data can of type \code{\link{data.frame}} or \code{\link{list}}
setMethod("initialize", signature="Node", function(.Object, data=NULL, ...) {
      
    li <- list(...)
    if(is.null(li$showwarnings))  li$showwarnings <- FALSE
    if(!is.null(data)){ 
        if(class(data) =="data.frame") {
            if(nrow(data) !=1) stop("Dataframes may only have one row for init.")
            li$x <- data$x
            li$y <- data$y
            li$id <- data$id
            li$label <- data$label 
        } else if(class(data) =="list") {
            
            duplicate.fields <- NULL
            for(j in 1:length(data)){
                l <- which(names(data)[j]==names(li))
                if(length(l) > 0){
                    duplicate.fields <- c(duplicate.fields,names(li)[l])
                }
            }
            duplicate.fields <- unique(duplicate.fields) 
            if(length(duplicate.fields)>0)  
                stop(paste("Cannot Construct Object. The field(s) '",
                        paste( 
                            duplicate.fields, 
                            collapse="', '", 
                            sep=""
                        ),
                        "' are given more than once.", 
                        sep=""
                     )
                )

            li<- append( data,li) 
        } else{ 
            stop("Error: argument data should be of type 'data.frame' or 'list'!")
        }
    }
    
    if(is.null(li$x)) {
        li$x <- as.numeric(runif(1,0,100)) 
        w <- paste("x-Coordinate simulated (x= ",li$x,").")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$x)!=1){
            stop("only 1 item for attribute x accepted.")
        } 
    } 
    if(is.null(li$y)) {
        li$y <- as.numeric(runif(1,0,100)) 
        w <- paste("y-Coordinate simulated (y= ",li$y,").")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$y)!=1){
            stop("only 1 item for attribute y accepted.")
        } 
    }

    if(is.null(li$id)) {
        li$id <- paste("n",sample(1:1000,1),sep="")
        w <- paste("Random ID (",li$id,") provided. Uniqueness may not be given.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$id)!=1){
            stop("only 1 item for attribute id accepted.")
        } 
    } 
    if(is.null(li$label)) {
        li$label <- li$id
        w <- paste("Random label (",li$label,") provided. Uniqueness may not be given.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$label)!=1){
            stop("only 1 item for attribute label accepted.")
        } 
    }

    .Object@x      <- as.numeric(  li$x)
    .Object@y      <- as.numeric(  li$y)
    .Object@id     <- as.character(li$id)
    .Object@label  <- as.character(li$label)
  
  
    if(validObject(.Object)) {
        return(.Object )
    }
})
################################### extract - Method ###################################################

#' @aliases $,Node-method 
#' @rdname extract-methods 
setMethod("$","Node",function(x,name) {return(slot(x,name))})

################################### Set - Method ###################################################

#' @aliases $<-,Node-method 
#' @rdname set-methods
setMethod("$<-","Node",function(x,name,value) {
  slot(x,name,check=TRUE) <- value
  valid<-validObject(x)
  return(x)
})

################################### Show - Method ###################################################
#' @title show Method 
#' @param object Object can be of type \code{\link{Node}}
#' @name show
#' @description Object can be of type \code{\link{Node}} 
#' @aliases show,Node-method 
#' @rdname show-methods 
setMethod ("show", "Node", function(object){
        cat("S4 class Node:")
        if(!is.null(object@id)) {
            cat("\tid:",object@id)
        }
        cat("\n\tisS4:  ",isS4(object),"\n")
        if(!is.null(object@label)) {
            cat("\tlabel: ",object@label, "\n")
        }
        if(!is.null(object@x) && !is.null(object@y)) {
            cat("\t(x/y): ","(",object@x,"/",object@y,")\n")
        } 
    }
) # end show method
################################### as... - Method ###################################################
as.Node.list = function(x, ...){return(new("Node", data=x))}
as.Node.data.frame = function(x, ...){return(new("Node", data=x))}
as.data.frame.Node = function(x, ...){
    li<-list(...)
    if(is.null(li$withrownames)) li$withrownames <- FALSE
    df<-data.frame(id=x@id, label = x@label, x= x@x, y= x@y)
    if(li$withrownames) rownames(df)<-x@id
    return (df)
}
as.list.Node = function(x, ...){list(id=x@id, label = x@label, x= x@x, y= x@y)}
 
setGeneric("as.Node",       function(x, ...) standardGeneric( "as.Node")) 
setGeneric("is.Node",       function(x, ...) standardGeneric( "is.Node")) 
 
setMethod("as.Node",     signature(x = "list"),       as.Node.list) 
setMethod("as.Node",    signature(x = "data.frame"),  as.Node.data.frame)  
setMethod("as.list",        signature(x = "Node"),        as.list.Node) 
setMethod("as.data.frame",  signature(x = "Node"),        as.data.frame.Node) 
 
setAs("data.frame", "Node", def=function(from){return(as.Node.data.frame(from))})
setAs("list", "Node", def=function(from){return(as.Node.list(from))})
################################### is... - Method ###################################################
setMethod( "is.Node", "Node", function(x, ...){return(is(x ,"Node"))})
 