#' @name TPP.SteppingStone.CalcPolyGonZug
#' @rdname TPP.SteppingStone.CalcPolyGonZug 
#' @title Transportation-Problem -- Stepping-Stone-Method -- Helper: CalcPolyGonZug
#'
#' @description Calculates the best Polygon-Cycle
#' @section WARNING:
#'	This function is meant for internal purposes only. It is not required to call this function manually at any time!
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param i numeric REQUIRED. Describes the pivot-Element the polygon-cycles should be searched for.
#' @param j numeric REQUIRED. Describes the pivot-Element the polygon-cycles should be searched for.
#' @param Poly list REQUIRED. Used only if the function is called recursively.
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TPP.SteppingStone.CalcPolyGonZug}}}{
#'    \describe{ 
#'      \item{log}{logical Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.} 
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
setGeneric("TPP.SteppingStone.CalcPolyGonZug",  function(object,i,j,Poly, ...)  standardGeneric("TPP.SteppingStone.CalcPolyGonZug") )

#' @aliases TPP.SteppingStone.CalcPolyGonZug,GeoSituation-method
#' @rdname TPP.SteppingStone.CalcPolyGonZug
 setMethod("TPP.SteppingStone.CalcPolyGonZug", signature(object="GeoSituation", i="integer", j="integer", Poly="list"),
  function(object,i,j,Poly,...){ 
  
		li <- list(...) 
 		  
	  	if(is.null(li$log)) li$log <- FALSE 
 
		n.Poly<-NULL
		x <- object$tpp.x
		cij <- object$tpp.costs

		if(li$log){ 
    		# message("\tTPP.SteppingStone.CalcPolyGonZug(i=",i,", j=",j," , ...)\n")
	  	}
	  	
		if(x[i,j]>0){
			#Wenn das Aktuelle Element eine Basisvariable ist, 
			#Erstelle einen neuen Polygonzug
			n.Poly<-Poly
			#Fuege die aktuelle Position als Element hinzu
			n.Poly$List<-rbind(n.Poly$List,data.frame(i=i,j=j))
			#und addiere/subtrahiere die Kosten 
			#zur Berecng der Opportunitaet
			# print(n.Poly$opp)
			if(n.Poly$NextIsAdd){
				n.Poly$opp <- n.Poly$opp + cij[i,j]
			}else{
				n.Poly$opp <- n.Poly$opp - cij[i,j]
			} 
			#Beim naechsten mal genau andersrum :)
			n.Poly$NextIsAdd <-!n.Poly$NextIsAdd
			n.Poly$vertical	 <-!n.Poly$vertical
			n.Poly$count	 <- n.Poly$count+1
			 
		}
		return(n.Poly)
	}
) 