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
#'   	\item{log}{logical Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.} 
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
            object$tpp$cij  <- cij
            
            I <- length(object$warehouses)
            J <- length(object$customers)
            
            #set supply and demand
            supply <- object$warehouses$supply
            demand <- object$customers$demand
            
            if(sum(supply) != sum(demand)) stop("This alg. can be used for non-degenerated solutions only: The Sums of warehouse$supply and customer$demand are not equal.")
            
            #set initial transportplan
            x <- getInitialMatrix(object, initialvalue = 0, ...)  # set initial Transportation Plan
            
            markedI <- rep(FALSE, I)
            markedJ <- rep(FALSE, J)
            
            while(TRUE){
              
              k<-NA
              l<-NA
              tmp<- sum(cij)*2+3 
              for(i in which(markedI==FALSE)){
                for(j in which(markedJ==FALSE)){
                  if(cij[i,j] < tmp){
                    k <- i
                    l <- j
                    tmp<-cij[i,j]
                  }
                }
              }
              if(is.na(k) | is.na(l)){
                stop("The TPP is not solveable with MMM if you can read this error. No next assignment could be made.")
              }
              x[k,l] <- min(supply[k], demand[l])
                          
              supply[k] <- supply[k] - x[k,l]
              demand[l] <- demand[l] -  x[k,l]
              
              if(supply[k] == 0 & (demand[l] >0  | markedJ[l] == FALSE)){
                markedI[k] <- TRUE
              }else{
                if(demand[l] == 0 & length(which(markedJ==FALSE) >1)){
                  markedJ[l] <- TRUE
                }
              } 
              if(length(which(markedI==FALSE)) ==0 & length(which(markedJ==FALSE)) == 1){
                break # CMM terminates.
              }
            } 
            object$tpp$x <- x 
            object$tpp$markedI <- markedI
            object$tpp$markedJ <- markedJ
            # check for M+N-1 variables.
            TPP.CheckValidTransportationPlan(object, ...) 
            return(object)
          }
)