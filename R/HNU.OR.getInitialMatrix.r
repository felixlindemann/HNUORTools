#' @title Caclulate Inital-Matrix
#' @name getInitialMatrix
#' @rdname getInitialMatrix 
#' @description  Calculates an inital NxM-matrix 
#' calls the function \code{\link{getDistance}(...)}
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{getInitialMatrix}}}{
#'    \describe{ 
#' 		\item{initialvalue}{\code{ANY} Optional Parameter for initial value. Default is \code{NA}.}  
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{...}} currently not forewarded.}
#'    }
#' }   
#' @keywords OR Initial-MAtrix
#' @export  
#' @seealso \code{\link{GeoSituation}}, \code{\link{Customer}}, \code{\link{Warehouse}} 
#' @examples
#' #  
##' @note 
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
#'			\url{http://felixlindemann.blogspot.de}
setGeneric("getInitialMatrix", function(object,...)  standardGeneric("getInitialMatrix") )
#' @aliases getInitialMatrix,GeoSituation,character,character-method
#' @rdname getInitialMatrix 
 setMethod("getInitialMatrix",signature(object="GeoSituation"),
  function(object,...){  
    li<-list(...) 

   	if(is.null(li$initialvalue))  li$initialvalue <- 0

	I <- length(object$warehouses)
	J <- length(object$customers)

	x <- matrix(rep( li$initialvalue , I*J), ncol = J, byrow = TRUE)

	rownames(x) <- sapply(object$warehouses, function(o){o$id})
	colnames(x) <- sapply(object$customers , function(o){o$id})
 
 
	return (x)
  }
)
