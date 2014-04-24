#' @name TPP.CMM
#' @rdname TPP.CMM 
#' @title Transportation-Problem -- Column-Minimum-Method
#'
#' @description Calculates the Transportation-Plan.
#' @param object Object of Type \code{\link{GeoSituation}} 
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TPP.CMM}}}{
#'    \describe{ 
#'   	\item{log}{logical Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}
#' 		\item{domschke.version}{numeric Optional Parameter. As Domschke describes two different implementations, the paremter \code{domschke.version} can take the values of the edition, in which the algorithm is described. \code{domschke.version} can take the values 2004 and 2007. Default Value is 2007. 
#'   	
#'   	  2004: Domschke, Wolfgang; Drexl, Andreas (2005): Einfuehrung in Operations Research. Mit 63 Tabellen. 6., ueberarb. und erw. Aufl. Berlin: Springer.
#'    
#'      2007: Domschke, Wolfgang (2007): Logistik. Transport. Grundlagen, lineare Transport- und Umladeprobleme. 5. Aufl. Muenchen: Oldenbourg (Oldenbourgs Lehr- und Handbuecher der Wirtschafts- und Sozialwissenschaften, 1).
#'     }
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
#' @keywords OR Transportation-Problem TPP Column-Minimum-Method
#' @return same (but modified) object of Type \code{\link{GeoSituation}}.
#'      The Solution will be assigned the attribute \code{tpp.x}.
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
setGeneric("TPP.CMM",  function(object,...)  standardGeneric("TPP.CMM") )

#' @aliases TPP.CMM,GeoSituation-method
#' @rdname TPP.CMM
setMethod("TPP.CMM", signature(object="GeoSituation"),
          function(object,...){ 
            
            li<-list(...)   
            object  <- TPP.Prepare(object, ...)			# repair degenerated if needed
            cij 	<- TPP.getCostMatrix(object, ...)	# store transportcosts localy
            
            if(is.null(li$domschke.version)) domschke.version <- 2007
            object$tpp$cij <- cij
            
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
            
            if(li$domschke.version == 2004){
                
              for(j in 1:J){
                while(markedJ[j] == FALSE){ 
                  #find mininmal Costs in Column
                  h<-NA
                  tmp <- sum(cij)*2+3
                  for(i in which(markedI==FALSE)){
                    
                    if( cij[i,j] < tmp){
                      
                      h<-i
                      tmp<-cij[i,j]
                    }
                    
                  }
                  if(is.na(k)) {stop("The TPP is not solveable with CMM if you can read this error. No next assignment could be made.")}
                  
                  x[h,j] <- min(supply[h], demand[j])
                  supply[h] <- supply[h]- x[h,j]
                  demand[j] <- demand[j] -  x[k,j]
                  
                  if(supply[h] == 0 ){
                    markedI[h] <- TRUE
                  }else{
                    if(demand[j] == 0 ){
                      markedJ[j] <- TRUE
                    }
                  }
                } 
              
              
              }
              
            }else if (li$domschke.version == 2007){
              
            
              while(TRUE){
                
                for(j in 1:J){
                  
                  # find next unmarked Column
                  if(markedJ[j] == FALSE){
                    
                    #find mininmal Costs in Column
                    k<-NA
                    tmp <- sum(cij)*2+3
                    for(i in which(markedI==FALSE)){
                      
                      if( cij[i,j] < tmp){
                        
                        k<-i
                        tmp<-cij[i,j]
                      }
                     
                    }
                    if(is.na(k)) {stop("The TPP is not solveable with CMM if you can read this error. No next assignment could be made.")}
                    
                    x[k,j] <- min(supply[k], demand[j])
                    supply[k] <- supply[k]- x[k,j]
                    demand[j] <- demand[j] -  x[k,j]
                    
                    if(supply[k] == 0 & (demand[j] >0  | markedJ[j] == FALSE)){
                      markedI[k] <- TRUE
                    }else{
                      if(demand[j] == 0 & length(markedJ[markedJ==FALSE]) >1){
                        markedJ[j] <- TRUE
                      }
                    }
                  } 
                } 
                if(length(which(markedI==FALSE)) ==0 & length(which(markedJ==FALSE)) == 1){
                  break # CMM terminates.
                }
              } 
            
            }else{
              msg<- paste("The given value for domschke.version is not supported. Please provide a value which is either 2004 or 2007. You provided: ", li$domschke.version)
              stop(msg)
            }
            object$tpp$x <- x 
            object$tpp$markedI <- markedI
            object$tpp$markedJ <- markedJ
            # check for M+N-1 variables.
            TPP.CheckValidTransportationPlan(object) 
            return(object)
          }
)