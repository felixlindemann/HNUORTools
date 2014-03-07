#' @name TPP.MODI.CalcOppCosts
#' @rdname TPP.MODI.CalcOppCosts 
#' @title Transportation-Problem -- MODI-Method -- Helper: CalcOppMatrix
#'
#' @description Calculates the Opportunity-Cost-Matrix
#' @section WARNING:
#'  This function is meant for internal purposes only. It is not required to call this function manually at any time!
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TPP.MODI.CalcOppCosts}}}{
#'    \describe{ 
#'      \item{log}{logical Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.} 
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{getInitialMatrix}}} 
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
#'      \url{http://felixlindemann.blogspot.de}
setGeneric("TPP.MODI.CalcOppCosts",  function(object,...)  standardGeneric("TPP.MODI.CalcOppCosts") )
 
#' @aliases TPP.MODI.CalcOppCosts,GeoSituation-method
#' @rdname TPP.MODI.CalcOppCosts
 setMethod("TPP.MODI.CalcOppCosts", signature(object="GeoSituation"),
  function(object,...){ 
  	li <- list(...)  
  	if(is.null(li$log)) 	 li$log 	 <- TRUE 
    tmpI <- 0

    if(li$log) message("\t\tTPP.MODI.CalcOppCosts\n")
    
    cij <- object$tpp.costs
    x <- object$tpp.x
  
  	opp<-getInitialMatrix(object, initialvalue=NA, ...)
    N<- length(object$customers)
    M<- length(object$warehouses)

    u<- rep(NA, M)
    v<- rep(NA, N)
    u[1] <- 0
    #
    CONTINUE <- TRUE
    xRichtung <- FALSE 
    # Die Ermittlung des Polygonzuges ist ein Wechsel aus der 
    # Suche in X-Richtung und der Suche in y-Richtung.
    while(CONTINUE) {
      CONTINUE <- FALSE

      # use formula
      # u(i) <- cij - vj
      # v(j) <- cij - ui
      if(xRichtung){
        for(j in 1:N){
          if(is.na(v[j])){
            for(i in 1:M){
              if(x[i,j] > 0 & !is.na(u[i])){
                v[j] <- cij[i,j] - u[i]
                break
              }
            }
          }
        } 
      }else{
        for(i in 1:M){
          if(is.na(u[i])){
            for(j in 1:N){
              if(x[i,j] >0 & !is.na(v[j])){
                u[i] <- cij[i,j] - v[j]
                break
              }
            }
          }
        } 
      }  
      # are all variables set?
      for(i in 1:M){
        if(is.na(u[i])){
          CONTINUE  <- TRUE
          break
        }
      }
      if(!CONTINUE){
        for(j in 1:N){
          if(is.na(v[j])){
            CONTINUE  <- TRUE
            break
          }
        }
      } 
      tmpI <- tmpI + 1
      xRichtung <- !xRichtung # next time other wa
      if(tmpI >= M*N +1) break
      if(!CONTINUE) break
    }
    # fill opp-costs matrix
    for(i in 1:M){
      for(j in 1:N){
        if(x[i,j] == 0 ){
          opp[i,j] <- cij[i,j] - u[i] - v[j]
        }
      }
    }        
    
    if(li$log){
      message("\t\tOpp Costs (MODI-Methode)\n")
      print(opp)
      message("\t\tu[i]\n")
      print(u)
      message("\t\tv[j]\n")
      print(v)
    }        
        
    object$tpp.costs.opp <- opp

	return(object)
  }  
)