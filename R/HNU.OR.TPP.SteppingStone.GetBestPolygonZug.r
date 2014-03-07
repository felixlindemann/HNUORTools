#' @name TPP.SteppingStone.GetBestPolygonZug
#' @rdname TPP.SteppingStone.GetBestPolygonZug 
#' @title Transportation-Problem -- Stepping-Stone-Method -- Helper: GetBestPolygonZug
#'
#' @description Calculates the best Polygon-Cycle
#' @section WARNING:
#'	This function is meant for internal purposes only. It is not required to call this function manually at any time!
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param i integer REQUIRED. Describes the pivot-Element the polygon-cycles should be searched for.
#' @param j integer REQUIRED. Describes the pivot-Element the polygon-cycles should be searched for.
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TPP.SteppingStone.GetBestPolygonZug}}}{
#'    \describe{ 
#' 		\item{log}{\code{"logical"} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}  
#' 		\item{Poly}{\code{"list"} \emph{Optional Parameter}. Used only if the function is called recursively.}
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{TPP.SteppingStone.GetPolygonZuege}}}
#'    }
#' }   
#' @keywords OR Transportation-Problem TPP Stepping-Stone
#' @details Explain what SSM does.
#' @export  
#' @references Domschke
#' @seealso \code{\link{GeoSituation}}, \code{\link{Node}}, \code{\link{TPP.NW}}, \code{\link{TPP.CMM}}, \code{\link{TPP.MMM}}, \code{\link{TPP.SteppingStone}}, \code{\link{TPP.MODI}}
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
setGeneric("TPP.SteppingStone.GetBestPolygonZug",  function(object,i,j,...)  standardGeneric("TPP.SteppingStone.GetBestPolygonZug") )

#' @aliases TPP.SteppingStone.GetBestPolygonZug,GeoSituation,integer,integer-method
#' @rdname TPP.SteppingStone.GetBestPolygonZug
 setMethod("TPP.SteppingStone.GetBestPolygonZug", signature(object="GeoSituation",i="integer", j="integer"),
  function(object,i,j,...){ 
 
  		li <- list(...)  
	  	if(is.null(li$log)) li$log <- TRUE 
	  	if(li$log){ 
    		# message("\tTPP.SteppingStone.GetBestPolygonZug\n")
	  	}
  

		# in case for one pair there exists more than one...
		# TPP.SteppingStone.GetPolygonZuege should return only one
  		 
		foo<-NULL
		foo<-TPP.SteppingStone.GetPolygonZuege(object,i_i = i,j_j =j)
		Poly<-NULL
		if (!is.null(foo)){
			Y<-nrow(foo)
			Poly<-foo[1,]
			if (Y>1){
				# Should NEVER be possible, if M + N -1 is hold.
				for (l in (2:Y)){ 
					n.Poly<-foo[l,]
					if(n.Poly$opp < Poly$opp){
						Poly<-n.Poly
					}
				}
			}
		}
		return(Poly)
	}
)