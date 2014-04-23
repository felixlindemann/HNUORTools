#' @name TPP.NW
#' @rdname TPP.NW 
#' @title Transportation-Problem -- North-West-Corner-Rule
#'
#' @description Calculates the Transportationplan.
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TPP.NW}}}{
#'    \describe{ 
#'   	\item{log}{\code{"logical"} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}  
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{getInitialMatrix}}}
#'      \item{\code{\link{TPP.Prepare}}}
#'    }
#' }  
#' @keywords OR Transportation-Problem TPP North-West-Corner-Rule
#' @export  
#' @references Domschke
#' @seealso \code{\link{GeoSituation}}, \code{\link{Node}}, \code{\link{TPP.NW}}, \code{\link{TPP.CMM}}, \code{\link{TPP.MMM}}, \code{\link{TPP.SteppingStone}}, \code{\link{TPP.MODI}}
#' @examples
#' # demo(HNUTPP03) 
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
setGeneric("TPP.NW",  function(object,...)  standardGeneric("TPP.NW") )

#' @aliases TPP.NW,GeoSituation-method
#' @rdname TPP.NW
setMethod("TPP.NW", signature(object="GeoSituation"),
          function(object,...){ 
            
            li<-list(...)  
            object <- TPP.Prepare(object, ...) 			# repair degenerated if needed
            x <- getInitialMatrix(object, initialvalue = 0, ...)  # set initial Transportation Plan
            
            #set supply and demand
            supply <- object$warehouses$supply
            demand <- object$customers$demand
            
            if(sum(supply) != sum(demand)) stop("This alg. can be used for non-degenerated solutions only: The Sums of warehouse$supply and customer$demand are not equal.")
             
            for(i in (1:length(object$warehouses))){ 
              for(j in (1:length(object$customers))){
                x[i,j] <- min(c(supply[i],demand[j]))
                supply[i] <- supply[i] - x[i,j]
                demand[j] <- demand[j] - x[i,j] 
              }
            } 
             
            object$tpp$x <- x  
            # check for M+N-1 variables.
            TPP.CheckValidTransportationPlan(object) 
            return(object)
          }
)