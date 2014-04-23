#' @name TPP.Prepare
#' @rdname TPP.Prepare 
#' @title Transportation-Problem -- Avoid degenerated Solutions
#'
#' @description prepare the setup of a TPP to be not degenerated. This function is called automatically when needed. Executing by User should not required.
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TPP.Prepare}}}{
#'    \describe{ 
#'   	\item{log}{\code{"logical"} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}  
#' 		\item{checkDegenerated}{\code{"logical"} Optional Parameter. Indicating, if the setup should be checked and corrected if neccassary. Default value is \code{TRUE}}  
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{...}}} currently not used and not forewarded to other functions.
#'    }
#' } 
#' @keywords OR Transportation-Problem TPP degenerated solutions
#' @export  
#' @return same modified object of Type \code{\link{GeoSituation}}.
#'     The setup may be corrected (a warning will be provided if so).
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
setGeneric("TPP.Prepare", function(object,...)  standardGeneric("TPP.Prepare") )

#' @aliases TPP.Prepare,GeoSituation-method
#' @rdname TPP.Prepare
setMethod("TPP.Prepare",signature(object="GeoSituation"),
          function(object,...){  
            li<-list(...) 
            
            if(is.null(li$checkDegenerated)) li$checkDegenerated <- TRUE
            if(is.null(li$log))  li$log  <- FALSE 
            
            if(li$checkDegenerated){
              
              supply <- sum(object$warehouses$supply)
              demand <- sum(object$customers$demand)
              
              
              if(demand < supply){ 
                
                object <- add(object, new("Customer", x =0, y=0, demand = supply-demand, id="dummy", label="dummy")) 
              if(li$log)  message(paste("Dummy Customer added (Less demand than supply)."))
              } else if( demand > supply){
                
                object <- add(object, new("Warehouse", x =0, y=0, supply = demand-supply, id="dummy", label="dummy"))  
                if(li$log)    message(paste("Dummy Warehouse added (Less supply than demand).")) 
              } 
            } 
            return (object)
          }
)
