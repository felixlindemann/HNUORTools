#' @exportClass Link
#' @name Link 
#' @rdname Link
#' @aliases Link-class 
#' @title The Link class
#' 
#'  @description
#' This class is part of the \pkg{HNUORTools}. It represents the connection of two \code{\link{Node}s} in an
#' \dfn{Operations-Research (OR)}-context.
#' 
#' @details Find here the defined slots for this class.
#' @section Slots defined:  
#' \describe{
#'    \item{\code{id}:}{
#'      Object of class \code{"character"}, containing data from id.
#'      \strong{Should be unique}.
#'      The default value will be caluclated randomly.
#'    }
#'    \item{\code{label}:}{
#'      Object of class \code{"character"}, containing the label of 
#'      the \code{\link{Node}}.
#'      The default value will be caluclated randomly.
#'    }
#'    \item{\code{origin}:}{
#'      Object of class \code{"Node"}, containing the origin \code{\link{Link}}. This value is REQUIRED. 
#'    }
#'    \item{\code{destination}:}{
#'      Object of class \code{"Node"}, containing the destination \code{\link{Link}}. This value is REQUIRED. 
#'    }
#'    \item{\code{costs}:}{
#'      Object of class \code{"numeric"}, containing the costs for using this \code{\link{Link}}.
#'      The default value will be caluclated automatically (if not provided during creation).
#'    }
#'    \item{\code{distance}:}{
#'      Object of class \code{"numeric"}, containing the distance for using this \code{\link{Link}}.
#'      The default value will be caluclated automatically (if not provided during creation).
#'    }
#'    \item{\code{oneway}:}{
#'      Object of class \code{"logical"}, indicating if this \code{\link{Link}} can be accessed both directions.
#'      The default value will is \code{FALSE}.
#'    }
#'    \item{\code{used}:}{
#'      Object of class \code{"logical"}, 
#'      Usually automatically assigned when solving the \dfn{Shortest-Path-Problem (SPP)}
#'      with the \code{SPP.Dijkstra}.
#'      The default value will is \code{FALSE}.
#'    }
#'  } 
#' @section Methods:
#' \describe{
#'    \item{\code{$}}{
#'      getting the value of a slot
#'    }
#'    \item{\code{$<-}}{
#'      assinging a value to of the slots
#'    }
#'  }
#'      @section To be used for:
#'          \describe{ 
#'              \item{SPP}{
#'                   Shortest-Path-Problem
#'              }
#'              \item{TSP}{
#'                  Travelling Salesman Problem
#'              }
#'          } 
#'    
#' @seealso \code{\link{Node}}
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
#' # demo(HNULink01)  
#'
setClass(
  Class = "Link",
  representation=representation(
    id     = "character",
    label  = "character",
    origin      = "Node",
    destination = "Node",
    costs       = "numeric",
    distance = "numeric",
    oneway = "logical",
    used = "logical"
  ),
  prototype=prototype(
    list(
      id    = character(),
      label = character(),
      origin     = NA,
      destination     = NA,
      costs = numeric(),
      distance = numeric(),
      oneway = logical() ,
      used = logical()
    )
  ),
  validity = function(object){
    
    
    if( sum(is.null(object@origin)) + sum(length(object@origin) ==0) > 0 ) {
      return(paste("Error with value origin: origin is not initialized", class(object@origin)))
    } 
    if( sum(is.null(object@destination)) + sum( length(object@destination) ==0) > 0) {
      return(paste("Error with value destination: destination is not initialized", class(object@destination)))
    } 
    if( !is.Node(object@origin) ) {
      return(paste("Error with value origin: expected Node, but obtained", class(object@origin)))
    } 
    if( !is.Node(object@destination) ) {
      return(paste("Error with value destination: expected Node, but obtained", class(object@destination)))
    } 
    if( length(object@origin  ) != 1 ){ 
      return(paste("Error with value origin: expected value of length 1, but obtained", length(object@origin)))
    } 
    if( length(object@destination  ) != 1 ){ 
      return(paste("Error with value destination: expected value of length 1, but obtained", length(object@destination)))
    } 
    if( length(object@distance  ) != 1 ){ 
      return(paste("Error with value distance: expected value of length 1, but obtained", length(object@distance)))
    } 
    if( length(object@costs  ) != 1 ){ 
      return(paste("Error with value costs: expected value of length 1, but obtained", length(object@costs)))
    }  
    if( object@distance < 0 ){ 
      return(paste("Error with value distance: expected non-negative value, but obtained", object@distance))
    } 
    if( object@costs < 0 ){ 
      return(paste("Error with value costs: expected non-negative value, but obtained", object@costs))
    } 
    return(TRUE)
  } 
)
################################### initialize - Method ###################################################
#' @title initialize Method
#' @name initialize
#' @aliases initialize,Link-method 
#' @rdname initialize-methods 
#' @param n1 Origin-\code{\link{Node}}
#' @param n2 Destination-\code{\link{Node}}
setMethod("initialize", "Link", function(.Object, n1,n2,... ) {
  
  li <- list(...)
  
  if(is.null(li$showwarnings))  li$showwarnings <- FALSE
  
  
  if(is.null(n1))  stop("No Origin node is provided.")  
  if(is.null(n2))  stop("No Destination node is provided.")  
  
  if(length(n1) !=1 ) stop("Incorrect Number of objects for Origin-Node. Expected is one.")
  if(length(n2) !=1 ) stop("Incorrect Number of objects for Destination-Node. Expected is one.")
  
  if(!is.Node(n1)) stop("The value for the origin Node is not of type Node")
  if(!is.Node(n2)) stop("The value for the destination Node is not of type Node")
  
  .Object@origin          <- n1
  .Object@destination     <- n2
  if(is.null(li$oneway)) {
    li$oneway <- FALSE 
    w <- paste("Attribute oneway set to default: FALSE.")
    if(li$showwarnings) warning(w) 
  }else{
    if(length(li$oneway)!=1){
      stop("only 1 item for attribute oneway accepted.")
    } 
  }
  if(is.null(li$used)) {
    li$used <- FALSE 
    w <- paste("Attribute used set to default: FALSE.")
    if(li$showwarnings) warning(w) 
  }else{
    if(length(li$used)!=1){
      stop("only 1 item for attribute used accepted.")
    } 
  }
  if(is.null(li$distance)) {
    li$distance <- getDistance(n1,n2, ...)
    w <- paste("automatic distance (",li$distance,") calculated.")
    if(li$showwarnings) warning(w) 
  }else{
    if(length(li$distance)!=1){
      stop("only 1 item for attribute distance accepted.")
    } 
  }
  if(is.null(li$costs)) { 
    li$costs <- getDistance(n1,n2,...) 
    w <- paste("automatic costs (",li$costs,") calculated.")
    if(li$showwarnings) warning(w) 
  }else{
    if(length(li$costs)!=1){
      stop("only 1 item for attribute costs accepted.")
    } 
  }
  
  if(is.null(li$id)) { 
      tmp.id <-  UUIDgenerate()
    li$id <- tmp.id
    w <- paste("Random ID (",li$id,") provided. Uniqueness should be given.")
    if(li$showwarnings) warning(w) 
  }
  if(is.null(li$label)) {
    li$label <- li$id 
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
  
  .Object@used     <- as.logical(li$used)
  .Object@oneway   <- as.logical(li$oneway)
  .Object@distance <- as.numeric(li$distance)
  .Object@costs    <- as.numeric(li$costs)
  .Object@label    <- as.character(li$label) 
  .Object@id       <- as.character(li$id) 
  
  
  if(validObject(.Object)) {
    return(.Object )
  }
})

################################### Extract - Method ###################################################
#' @title Extract Method
#' @name $
#' @aliases $,Link-method 
#' @rdname Extract-methods-1 
setMethod("$","Link",function(x,name) {return(slot(x,name))})
################################### Set - Method ###################################################
#' @title Set Method 
#' @name $<-
#' @aliases $<-,Link-method 
#' @rdname Set-methods-1
setMethod("$<-","Link",function(x,name,value) {
  slot(x,name,check=TRUE) <- value
  valid<-validObject(x)
  return(x)
}) 
#' @title show Method
#' @name show
#' @aliases show,Link-method 
#' @description Object can be of type \code{\link{Link}} 
#' @param object Object can be of type \code{\link{Link}} 
#' @rdname show-methods 
setMethod ("show", "Link", function(object){
  cat("S4 class Link: (",object@id," '",object@label,"')\n")
  cat("\tOrigin:", object@origin@label)
  if(!object@oneway) cat("<-")
  cat("-> Destination:", object@destination@label) 
  cat("\tdistance:", object@distance)
  cat("\tcosts:", object@costs)  
  cat("\n")
}
) # end show method
################################### as... - Method ###################################################

as.Link.list = function(x, ...){return(new("Link", x))}
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
as.Link.data.frame = function(x, ...){return(new("Link", x))}
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
  if(li$withrownames) rownames(df)<-x@id
  return (df)
} 
#
#
setGeneric("as.Link",       function(x, ...) standardGeneric( "as.Link")) 
setGeneric("is.Link",       function(x, ...) standardGeneric( "is.Link")) 

setMethod("as.Link",        signature(x = "list"),       as.Link.list) 
setMethod("as.Link",        signature(x = "data.frame"),  as.Link.data.frame)  
setMethod("as.list",        signature(x = "Link"),        as.list.Link) 
setMethod("as.data.frame",  signature(x = "Link"),        as.data.frame.Link) 
# 
setAs("data.frame", "Link", def=function(from){ return(as.Link.data.frame(from))})
setAs("list", "Link", def=function(from){return(as.Link.list(from))})
################################### is... - Method ###################################################
setMethod( "is.Link", "Link", function(x, ...){return(is(x ,"Link"))})

