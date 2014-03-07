#' @name TSP.3OPT 
#' @rdname TSP.3OPT 
#' @title Travelling-Salesman-Problem -- Three-Opt
#'
#' @description Improves a given Route by switching links.
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TSP.3OPT}}}{
#'    \describe{ 
#' 		\item{log}{\code{"logical"} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}  
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{...}} currently not forewarded.}
#'    }
#' }  
#' @keywords OR Travelling-Salesman-Problem TSP 3-opt Three-opt
#' @details Explain what Three-opt does.
#' @export  
#' @return same modified object of Type \code{\link{GeoSituation}}.
#'      The Solution will be assigned the attribute \code{tsp.solution}.
#' @references Domschke
#' @seealso \code{\link{GeoSituation}}, \code{\link{Node}}, \code{\link{TSP.NearestNeighbor}}, \code{\link{TSP.2OPT}},  \code{\link{TSP.3OPT}}
#' @examples
#' # demo(HNUTSP01)
#' # demo(HNUTSP02) 
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
setGeneric("TSP.3OPT",  function(object,...)  standardGeneric("TSP.3OPT") )

#' @aliases TSP.3OPT,GeoSituation-method
#' @rdname TSP.3OPT
 setMethod("TSP.3OPT", signature(object="GeoSituation"),
  function(object,...){ 
    message("TSP.3OPT\n")
  	li <- list(...) 

	found<-TRUE
	if(is.null(li$log))  li$log  <- FALSE
	if(is.null(li$plot)) li$plot <- FALSE
	
	R   <- object$tsp.solution$roundtrip 
	n   <- length(R)-1
	cij <- object$tsp.solution$cij
	if(li$log){
		cat("\tNeue Route:\n")
		print(R)
	}
	while(found){
		found <- FALSE 
		for(zaehler in 1:n){
			for(h in 1:(n-3)){
				for(j in (h+1):(n-1)){

					d <- cij[ R[n],R[1] ] + cij[ R[h],R[h+1]] + cij[ R[j],R[j+1]]

					d1<- cij[ R[h],R[j+1] ]+ cij[ R[1],R[j] ]
					d2<- cij[ R[1],R[j+1] ] + cij[ R[h],R[j] ]

					T<-matrix(
						c(R[n], R[1], R[h],R[h+1],R[j],R[j+1]),
						ncol=2,byrow=TRUE)

					if(d1<=d2){ 

						if(d1 + cij[ R[h+1],R[n] ]<d){
							found<-TRUE
							R <- c(R[1:h],R[(j+1):n],R[(h+1):j],R[1])
							Variante<-1
						}
					}else{
						if(d2 + cij[ R[h+1],R[n] ]<d){
							found <- TRUE
							R<- c(R[1],R[(j+1):n], R[(h+1):j], R[h:1])
							Variante<-2
						}
						
					} 
					if(found==TRUE){
						break
					}
				}
				if(found==TRUE){
					x<-cij*0
					for(k in 2:length(R)){
						x[R[k-1],R[k] ]<-1
					}
					object$tsp.solution$F<-sum(cij*x) 
					object$tsp.solution$x<-x
					object$tsp.solution$roundtrip<-R
					break
				}else{
					R<-c(R[n], R[1:(n-1)],R[n]) 
				}
			} 
			if(found==TRUE){
				break
			}
		}  
	}
	return(object)
  }
 )