#' @name TPP.CheckValidTransportationPlan
#' @rdname TPP.CheckValidTransportationPlan
#' @title Transportation-Problem -- Avoid non-Basis-Solution (M+N-1 Basisvariables)
#'
#' @description Check if M+N-1 basis-variables exists.
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TPP.Prepare}}}{
#'    \describe{ 
#'     \item{log}{\code{"logical"} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}  
#' 		\item{checkValididty}{\code{"logical"} Optional Parameter. Indicating, if the setup should be checked. Default value is \code{TRUE}. If not valid, an error is thrown.}  
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{...}}} currently not used and not forewarded to other functions.
#'    }
#' } 
#' @keywords OR Transportation-Problem TPP 
#' @export  
#' @return \code{\link{logical}}
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
setGeneric("TPP.CheckValidTransportationPlan", function(object,...)  standardGeneric("TPP.CheckValidTransportationPlan") )

#' @aliases TPP.CheckValidTransportationPlan,GeoSituation-method
#' @rdname TPP.CheckValidTransportationPlan
setMethod("TPP.CheckValidTransportationPlan",signature(object="GeoSituation"),
          function(object,...){  
            li<-list(...) 
            
            if(is.null(li$checkValididty)) li$checkValididty <- TRUE
            
            
            if(li$checkValididty){
               x<- object$tpp$x
               M <- length(object$warehouses)
               N <- length(object$customers)
               
               l <- length(which(x >0))
               if (l != M + N - 1){ 
                 s<-paste("The calculated Transportation-Plan is not a Basis-Solution (M+N-1 Variables).\n",
                          "expected are ", (M+N-1), "provided are", l , "variables.\n",
                          "It is not recommended to use this for beginner students studies.")
                 stop(s)
                 return (FALSE)
               } 
            }  
           
          return(TRUE)
          }
) 