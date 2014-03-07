
#' @aliases $,Node-method 
#' @rdname extract-methods 
setMethod("$","Node",function(x,name) {return(slot(x,name))})

#' @aliases $<-,Node-method 
#' @rdname set-methods
setMethod("$<-","Node",function(x,name,value) {
  slot(x,name,check=TRUE) <- value
  valid<-validObject(x)
  return(x)
})

 

#' @title distance between two \code{\link{Node}s}
#' @description Calculates the distance between to nodes.
#' @param n1 an object of class \code{"Node"} or a class derived from \code{"Node"}.
#' @param n2 an object of class \code{"Node"} or a class derived from \code{"Node"}.
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{calc.Distance}}}{
#'    \describe{ 
#'      \item{log}{\code{"logical"} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}
#'      \item{costfactor}{\code{"numeric"} Optional Parameter. Used to transform the distance by a factor into costs. Default is \code{1}.}  
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{...}} currently not used and not forewarded. } 
#'    }
#' } 
#' @name calc.Distance 
#' @docType methods
#' @rdname calc.Distance
##' @author Dipl. Kfm. Felix Lindemann \email{felix.lindemann@@hs-neu-ulm.de} 
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
setGeneric("calc.Distance", function(n1,n2,   ...) standardGeneric("calc.Distance"))
setMethod( "calc.Distance", signature(n1 = "Node", n2 = "Node"),
    function(n1,n2,   ...) {
        li<-list(...)

        if(!is.Node(n1)) stop("Node 1 is not of type Node")
        if(!is.Node(n2)) stop("Node 2 is not of type Node")
        validObject(n1)
        validObject(n2)
        
        if(is.null(li$costfactor)) li$costfactor <-1

        dist <- sqrt( (n1@x - n2@x)^2 + (n1@y - n2@y)^2 ) * li$costfactor
        return(dist)
    }
)


#' @title Calculate angle of a link connecting two \code{\link{Node}s}
#' @description Calculates the  angle of a link connecting two \code{\link{Node}s}.
#' @param n1 an object of class \code{"Node"} or a class derived from \code{"Node"}.
#' @param n2 an object of class \code{"Node"} or a class derived from \code{"Node"}.
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{calc.polar}}}{
#'    \describe{ 
#'      \item{...}{No parameters are taken directly.} 
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{getPhi}}} 
#'    }
#' } 
#' @name calc.polar 
#' @docType methods
#' @rdname calc.polar
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
setGeneric("calc.polar", function(n1,n2,   ...) standardGeneric("calc.polar"))
setMethod( "calc.polar", signature(n1 = "Node", n2 = "Node"),
    function(n1,n2,   ...) {
        li<-list(...)

        if(!is.Node(n1)) stop("Node 1 is not of type Node")
        if(!is.Node(n2)) stop("Node 2 is not of type Node")
        validObject(n1)
        validObject(n2)
        
        value <- getPhi(x = n2$x, y=n2$y, x0=n1$x, y0 = n1$y, ...)
        
        return(value)
    }
)
