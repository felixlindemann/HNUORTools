#' @name TPP.VOGEL
#' @rdname TPP.VOGEL 
#' @title Transportation-Problem -- VogelAproximation Method
#'
#' @description Calculates the Transportation-Plan.
#' @param object Object of Type \code{\link{GeoSituation}} 
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TPP.VOGEL}}}{
#'    \describe{ 
#'     \item{log}{logical Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}
#' 		 
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
#' @references Domschke, Wolfgang; Drexl, Andreas (2005): Einfuehrung in Operations Research. Mit 63 Tabellen. 6., ueberarb. und erw. Aufl. Berlin: Springer.

#' @seealso \code{\link{GeoSituation}}, \code{\link{Node}}, \code{\link{TPP.NW}}, \code{\link{TPP.VOGEL}}, \code{\link{TPP.MMM}}, \code{\link{TPP.SteppingStone}}, \code{\link{TPP.MODI}}
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
setGeneric("TPP.VOGEL",  function(object,...)  standardGeneric("TPP.VOGEL") )

#' @aliases TPP.VOGEL,GeoSituation-method
#' @rdname TPP.VOGEL
setMethod("TPP.VOGEL", signature(object="GeoSituation"),
          function(object,...){ 
            
            li<-list(...)   
            object  <- TPP.Prepare(object, ...)			# repair degenerated if needed
            cij 	<- TPP.getCostMatrix(object, ...)	# store transportcosts localy
             
            if(is.null(li$log)) li$log <- FALSE
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
            iteration <- 1
             
            dz <- matrix(rep(NA,I), ncol=1)
            ds <- matrix(rep(NA,J), ncol=J) 
            
            
            while(TRUE){
              if(li$log)  cat("entering iteration:", iteration, "\n")
              
              #1. Calc for each row i the difference cih - cik between 
              # the smallest and second smallest element cih and the 
              # smallest cik of elements in all umarked columns in row i
             
              for(i in 1:I){
                if(markedI[i] == FALSE){
                  cik <- sum(cij) *2 +1
                  cih <- sum(cij) *2 +1
                  k <- NA
                  h <- NA
                  #find smallest
                  for(j in 1:J){
                    if(markedJ[j]==FALSE){
                      if(cik > cij[i,j]){
                        cik <- cij[i,j]
                        k<- j
                      }
                    }
                  }
                  #find second smallest
                  for(j in 1:J){
                    if(markedJ[j]==FALSE & j !=k){
                      if(cih > cij[i,j]){
                        cih <- cij[i,j]
                        h<- j
                      }
                    }
                  }
                  dz[i,iteration] <- cih-cik
                }
              }
              #2. Calc for each column j the difference dsj = chj - ckj
              # between the secondsmallest chj and the smallest ckj element
              # of all unmarked rows for column j
              for(j in 1:J){
                if(markedJ[j] == FALSE){
                  
                  ckj <- sum(cij) *2 +1
                  chj <- sum(cij) *2 +1
                  k <- NA
                  h <- NA
                  #find smallest
                  for(i in 1:I){
                    if(markedI[i] == FALSE){
                      if(ckj > cij[i,j]){
                        ckj<-cij[i,j]
                        k<-i
                      } 
                    }
                  }
                  for(i in 1:I){
                    if(markedI[i] == FALSE & i!=k){
                      if(chj > cij[i,j]){
                        chj<-cij[i,j]
                        h<-i
                      } 
                    }
                  } 
                  ds[iteration, j] <- chj-ckj
                } 
              }
              
              if(li$log) message("ds")
              if(li$log) print(ds)
              if(li$log) message("dz")
              if(li$log) print(dz)
              
              #getMaximum
              m<-max(ds[iteration, ], dz[,iteration], na.rm = TRUE) 
              p<-NA
              q<-NA
              cpq <- NA
              for(i in 1:I){
                if(markedI[i] ==FALSE) {
                  if(dz[i,iteration] == m){
                    p<-i
                    break
                  }
                }
              }
              if(is.na(p)){
                for(j in 1:I){
                  if(markedJ[j] ==FALSE ){
                    if( ds[ iteration , j] == m){
                      q<-j
                      break
                    }
                  }
                }
                if(li$log) cat("found q:", q, "\n")
                cpq <- sum(cij) *2 +1        
                for(i in 1:I){
                  if(markedI[i] == FALSE){
                    if(cij[i,q] < cpq){
                      cpq <- cij[i,q]
                      p <- i
                    } 
                  }
                }  
              }else{
                if(li$log) cat("found p:", p, "\n")
                cpq <- sum(cij) *2 +1       
                for(j in 1:J){
                  if(markedJ[j] == FALSE){
                    if( cij[p,j] < cpq){
                      cpq <- cij[p,j]
                      q <- j
                    } 
                  }
                } 
              }
              if(li$log)  cat("found q:", q, "\n")
              
              #4.a Set Value
              x[p,q] <- min(supply[p], demand[q])
              supply[p] <- supply[p] - x[p,q]
              demand[q] <- demand[q] - x[p,q]
              
              #4.b Mark Rows/Columns
              if(supply[p] == 0){
                markedI[p] <- TRUE
              }else{
                markedJ[q] <- TRUE
              }
              
              # abort
              if(length(which(markedI)) == I-1 ){
                for(i in 1:I){
                  if(markedI[i] == FALSE){
                    for(j in 1:J){
                      if(markedJ[j] == FALSE){
                        x[i,j] <- demand[j]
                        supply[i] <- supply[i] - x[i,j]
                        demand[j] <- 0 
                      }
                    }
                    break
                  }
                } 
                break
              }
              if(length(which(markedJ)) == J-1){
                for(j in 1:J){
                  if(markedJ[j] == FALSE){
                    for(i in 1:I){
                      if(markedI[i] == FALSE){
                        x[i,j] <- supply[i]
                        supply[i] <- 0
                        demand[j] <- demand[j] - x[i,j] 
                      }
                    }
                    break
                  }
                } 
                break
              }
              dz<- cbind(dz, matrix(rep(NA,I), ncol=1))
              ds<- rbind(ds, matrix(rep(NA,J), ncol=J))
              iteration <- iteration + 1
            }
            
            object$tpp$x <- x 
            object$tpp$markedI <- markedI
            object$tpp$markedJ <- markedJ
            object$tpp$vogeldz<-dz
            object$tpp$vogelds<-ds
            object$tpp$iteration<-iteration
            if(li$log) message("Results from Vogel")
            if(li$log) print(object$tpp)
            # check for M+N-1 variables.
            TPP.CheckValidTransportationPlan(object) 
            return(object)
          }
)