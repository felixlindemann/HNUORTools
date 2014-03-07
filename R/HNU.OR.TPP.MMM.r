#' @name TPP.MMM
#' @rdname TPP.MMM 
#' @title Transportation-Problem -- Matrix-Minimum-Method
#'
#' @description Calculates the Transportationplan.
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TPP.MMM}}}{
#'    \describe{ 
#' 		\item{log}{logical Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.} 
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{getInitialMatrix}}}
#'      \item{\code{\link{TPP.Prepare}}}
#'      \item{\code{\link{TPP.getCostMatrix}}}
#'    }
#' } 
#' @keywords OR Transportation-Problem TPP Matrix-Minimum-Method
#' @export  
#' @return same modified object of Type \code{\link{GeoSituation}}.
#'      The Solution will be assigned the attribute \code{tpp.x}.
#' @references Domschke
#' @seealso \code{\link{GeoSituation}}, \code{\link{Node}}, \code{\link{TPP.NW}}, \code{\link{TPP.CMM}}, \code{\link{TPP.MMM}}, \code{\link{TPP.SteppingStone}}, \code{\link{TPP.MODI}}
#' @examples
#' # demo(HNUTPP03)
#' # demo(HNUTPP04)
#' # demo(HNUTPP05)
#' # demo(HNUTPP06)
#' # demo(HNUTPP07)
#' # demo(HNUTPP08)
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
setGeneric("TPP.MMM",  function(object,...)  standardGeneric("TPP.MMM") )
#' @aliases TPP.MMM,GeoSituation-method
#' @rdname TPP.MMM
 setMethod("TPP.MMM", signature(object="GeoSituation"),
  function(object,...){ 
    
    li<-list(...)  
  	object <- TPP.Prepare(object, ...) 			# repair degenerated if needed
	cij    <- TPP.getCostMatrix(object, ...)		# store transportcosts localy
	object$tpp.costs <- cij
	
	I <- length(object$warehouses)
	J <- length(object$customers)
	
	#set supply and demand
  	supply <- sapply(object$warehouses, function(o){o$supply})
	demand <- sapply(object$customers , function(o){o$demand})

	if(sum(supply) != sum(demand)) stop("This alg. can be used for non-degenerated solutions only: The Sums of warehouse$supply and customer$demand are not equal.")

	#set initial transportplan
	x <- getInitialMatrix(object, ...)  # set initial Transportation Plan
	
   	while(sum(demand)>0){	

   		ij <- NULL
		c.min<- max(cij) * 2 +1	
		for(i in 1:I){
			if(supply[i]>0){
				for(j in 1:J){
					if(demand[j]> 0){
						if(cij[i,j]<c.min) {
							ij <- c(i,j)
							c.min <- cij[i,j]
						}
					}
				}
			}
		}

		if(!is.null(ij)){
			i<- ij[1]
			j<- ij[2]
			x[i,j] <- min(c(supply[i],demand[j]))
			supply[i] <- supply[i] - x[i,j]
			demand[j] <- demand[j] - x[i,j] 
		}else{
			stop("this should not happen.")
		}
		if(sum(demand) + sum(supply) == 0) break
   	}
 
	object$tpp.x <- x   
	return(object)
  }
)