#' @name TPP.SteppingStone.CalcOppMatrix
#' @rdname TPP.SteppingStone.CalcOppMatrix 
#' @title Transportation-Problem -- Stepping-Stone-Method -- Helper: CalcOppMatrix
#'
#' @description Calculates the Opportunity-Cost-Matrix
#' @section WARNING:
#'  This function is meant for internal purposes only. It is not required to call this function manually at any time!
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TPP.SteppingStone.CalcOppMatrix}}}{
#'    \describe{ 
#'      \item{log}{logical Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.} 
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{getInitialMatrix}}}  
#'      \item{\code{\link{TPP.SteppingStone.GetBestPolygonZug}}}  
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
setGeneric("TPP.SteppingStone.CalcOppMatrix",  function(object,...)  standardGeneric("TPP.SteppingStone.CalcOppMatrix") )

#' @aliases TPP.SteppingStone.CalcOppMatrix,GeoSituation-method
#' @rdname TPP.SteppingStone.CalcOppMatrix
setMethod("TPP.SteppingStone.CalcOppMatrix", signature(object="GeoSituation"),
          function(object,...){  
            li <- list(...)  
            if(is.null(li$log)) li$log <- TRUE 
            if(li$log){ 
              message("\tTPP.SteppingStone.CalcOppMatrix")
            }
            M<- length(object$warehouses)
            N<- length(object$customers) 
            countBasisVariables <- M+N-1
            opp<-getInitialMatrix(object, initialvalue=NA, ...)
            x<-object$tpp$x
            
            for (i in 1:M){
              for (j in 1:N){
                if ((x[i,j]==0)) { 
                  p<-TPP.SteppingStone.GetBestPolygonZug(object,i = i,j = j)  
                  if(!is.null(p)){ 
                    opp[i,j]<-p$opp
                  }else{
                    message(paste("A Polygon-Cycle should have been calculated (x[i,j]:",x[i,j]," - i:",i,"/j:",j,")."))
                    cat("calculated OPP.Cost matrix:\n")
                    print(opp)
                    cat("current Transportplan:\n")
                    print(object$tpp$x)
                    stop("A Polygon-Cycle should have been calculated (x[i,j]:",x[i,j]," - i:",i,"/j:",j,").")
                  }
                }
              }
            } 
            if(sum(is.na(opp)) != countBasisVariables){
              message("ERROR in TPP.SteppingStone.CalcOppMatrix.")
              
              res <- matrix(rep("OK", M*N), ncol = N)
              for(i in 1:M){
                for(j in 1:N){
                  if( object$tpp$x[i,j] != 0 & !is.na(opp[i,j]) ){
                    res[i,j] <- "should Not be OPP"
                  }else if (object$tpp$x[i,j] == 0 &  is.na(opp[i,j])){
                    res[i,j] <- "should be OPP"
                  }
                }
              }
              cat("Errors in OPP.Cost matrix:\n")
              print(res)
              cat("calculated OPP.Cost matrix:\n")
              print(opp)
              cat("current Transportplan:\n")
              print(object$tpp$x)
              
              
              stop(paste("The number of opportunity costs is", sum(!is.na(opp)), 
                         "and not as expected", (M*N -countBasisVariables),"!\n",
                         "This indicates a bug in 'TPP.SteppingStone.CalcOppMatrix'."))
            }
            
            object$tpp$oppcosts<-opp
            
            if(li$log){ 
              message(paste("\t\tOpp-Cost-Matrix: \n"))
              print(opp) 
              cat("\n")
            }
            return(object)
          } 
)