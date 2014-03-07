#' @name TPP.getCostMatrix
#' @rdname TPP.getCostMatrix 
#' @title Transportation-Problem -- Column-Minimum-Method
#'
#' @description Calculates the Transportationplan.
#' @param object Object of Type \code{\link{GeoSituation}} 
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TPP.getCostMatrix}}}{
#'    \describe{ 
#' 		\item{log}{\code{logical} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.} 
#' 		\item{transportcosts}{\code{numeric} Optional Parameter. \strong{OBSOLETE}. Default is \code{1}.} 
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{getDistanceMatrix}}} 
#'    }
#' }
#' @keywords OR Transportation-Problem TPP Column-Minimum-Method
#' @return a MxN Matrix (Cij- For Transportation Problem)
#' @export  
#' @references Domschke
#' @seealso \code{\link{GeoSituation}}, \code{\link{Node}}, \code{\link{TPP.NW}}, \code{\link{TPP.CMM}}, \code{\link{TPP.MMM}}, \code{\link{TPP.SteppingStone}}, \code{\link{TPP.MODI}}
#' @examples
#' # demo(HNUTPP02)
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
#'			\url{http://felixlindemann.blogspot.de}
setGeneric("TPP.getCostMatrix", function(object,...)  standardGeneric("TPP.getCostMatrix") )
#' @aliases TPP.getCostMatrix,GeoSituation-method
#' @rdname TPP.getCostMatrix
 setMethod("TPP.getCostMatrix",signature(object="GeoSituation"),
  function(object,...){  
    li<-list(...) 

    if(!is.null(li$transportcosts)) warning("The use of the attribute transportcosts is obsolete. Please use the forwarded attribute 'costfactor' of method 'getdistance' for consistency reasons.")
	
	if(is.null(li$transportcosts)) li$transportcosts <- 1
	if(is.null(li$log)) li$log <- FALSE

	I <- length(object$warehouses)
	J <- length(object$customers)
	demand <- sapply(object$customers , function(o){o$demand})
	cij <- li$cij		
	if(is.null(cij)){	
		cij <- object$tpp.costs

		if(
			length(cij) == 1 
			| nrow(cij)	!= I 
			| ncol(cij)	!= J 
			| sum(is.na(cij))>0
		){ 


			cij <- getDistanceMatrix(object,"warehouses", "customers", ...)*li$transportcosts 
     		
		}  
	}  
	if(class(cij) != "matrix")  stop(paste("The object 'cij' is of type",class(cij),"- expected is matrix."))
	if(nrow(cij)!= I) 			stop(paste("The number of rows of object 'cij' is", nrow(cij),"- expected was", I))
	if(ncol(cij)!= J) 			stop(paste("The number of cols of object 'cij' is", ncol(cij),"- expected was", J))
	if(min(cij)<0) 				stop("Negative Values are not permitted in cij.")
	if(sum(is.na(cij))>0)		stop("NAs are not permitted in cij.") 
	 

	return (cij)
  }
)
