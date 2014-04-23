#' @name WLP.ADD 
#' @rdname WLP.ADD 
#' @title Warehouse-Location-Problem -- ADD-Algorithm
#'
#' @description Improves a given Route by switching links.
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{WLP.ADD}}}{
#'    \describe{ 
#'   	\item{log}{\code{"logical"} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}
#' 		\item{maxiter}{\code{"numeric"} Optional Parameter. if \code{maxiter} is a positive value, the algorithm terminates after \code{maxiter}-iterations.} 
#'		\item{cij}{\code{"matrix"} \emph{Optional Parameter}. use for a user-defined cij-matrix.}
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{...}} currently not used and not forewarded. } 
#'    }
#' } 
#' @keywords OR Warehouse-Location-Problem WLP ADD-Algorithm
#' @details Explain what ADD-Algorithm does.
#' @return same modified object of Type \code{\link{GeoSituation}}.
#'      The Solution will be assigned the attribute \code{WLP.solution}.
#' @export  
#' @references Algorithmus 3.1 Domschke, Wolfgang (1996): Logistik. Standorte. 4. Aufl. Muenchen: Oldenbourg (3)  S.62
#' @seealso \code{\link{GeoSituation}}, \code{\link{Node}},\code{\link{Warehouse}},\code{\link{Customer}}, \code{\link{WLP.ADD}}
#' @examples
#' # demo(HNUWLP01) 
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
setGeneric("WLP.ADD",  function(object,...)  standardGeneric("WLP.ADD") )

#' @aliases WLP.ADD,GeoSituation-method
#' @rdname WLP.ADD
setMethod("WLP.ADD", signature(object="GeoSituation"),
          function(object,...){ 
            message("This is .OR.WLP.ADD")
            li<-list(...)   
            
            if(is.null(li$cij)) li$cij <- object$wlp$cij 
            
            if(length(li$cij) ==1){
              stop("no costs matrix given. Problem not solveable.")
            }
            I <- length(object$warehouses)
            J <- length(object$customers)
            if(nrow(li$cij)!=I){
              stop("The number of rows in the given costs-matrix is wrong. (Expected:",I," given:",nrow(li$cij),")")
            }
            if(ncol(li$cij)!=J){
              stop("The number of columns in the given costs-matrix is wrong. (Expected:",J," given:",ncol(li$cij),")")
            }
            
            if(is.null(li$maxiter)) li$maxiter <- I * 2 +1 # make sure, it will not abort early
            if(is.null(li$log)) li$log <- FALSE
            
            if(li$log){
              
              cat("############ Initial Iteration: ############\n\nUsing cij:\n")
              print(li$cij)
            }
            object$wlp$cij <- li$cij
            
            # prepare result as list 
            result <- list()
            result$Iteration<-0 
            
            result$x <-object$wlp$cij*0
            result$y <-rep(0,I)
            
            #verwerfe alle Laeger vorlaeufig
            result$IOVL<-1:I 	#init Vorlaeuig  verworfene
            result$IO<-integer()   	#init endgueltig verworfene
            result$I1<-integer()		#init endgueltig aufgenommene
            
            
            #bereite w-Matrix vor
            result$w<-data.frame(matrix(rep(0,I*(J+3)),ncol=J+3))
            rownames(result$w)<-  object$warehouses$id
            colnames(result$w)<-c(object$customers$id,"wi","fi","wifi")
            result$w$fi<-object$warehouses$fixcosts
            
            
            #bereite cij-Matrix vor
            result$cij<-data.frame(matrix(rep(0,I*(J+3)),ncol=J+3))
            rownames(result$cij)<-  object$warehouses$id
            colnames(result$cij)<-c(object$customers$id,"ci","fi","cifi")
            result$cij$fi<-object$warehouses$fixcosts
            result$cij[,1:J] 	<- 	object$wlp$cij
            result$cij$ci 	 	<-	rowSums(object$wlp$cij)
            result$cij$cifi 	<-	result$cij$ci + result$cij$fi
            
            # suche das guenstigste Lager fuer alleinige Belieferunge
            tmp <- max(result$cij$cifi)*2+1
            k <- NA
            for(i in 1:I){
              object$warehouses$open[i] <- FALSE
              if(result$cij$cifi[i] < tmp){
                tmp <- result$cij$cifi[i]
                k <- i
              }
            } 
            if(is.na(i)){
              stop("This shouldn't happen. could not find cheapest warehouse.")
            }
            
            # Setzte I1
            result$I1 <- c(result$I1, k)
            # update IOVL
            result$IOVL <- result$IOVL[-which(result$IOVL== k )]
            #setze gesamtkosten
            result$totalcosts <- tmp
            
            #setze Ergebnisse
            result$y[k] <- TRUE
            result$x[k,] <- 1
            object$warehouses$open[k] <- TRUE
            
            object$wlp$x <- result$x
            object$wlp$y <- result$y 
            object$wlp$F <- result$totalcosts
            object$wlp$I1 <- result$I1
            object$wlp$I1VL <- result$I1VL
            object$wlp$I0VL <- result$IOVL
            object$wlp$I0 <- result$IO
            object$wlp$Iteration <- result$Iteration
            
            if(li$log){
              
              cat("calculated C(ij)-Matrix:\n")
              print(result$cij)
              cat("chosen Warehouse: ", k, "(k.min=",tmp,")\n")
              cat("defined sets\n")
              cat("\tI1:   {", result$I1, "}\n")
              cat("\tIO:   {", result$IO, "}\n")
              cat("\tIOVL: {", result$IOVL, "}\n")
              
              cat("Current Soultion x(ij)-Matrix:\n")
              print(result$x)
              
              cat("Current Soultion y(i):\n")
              print(result$y)
              
            }	 
            # iteriere solange, wie Laeger vorlaeufig verworfen sind
            while(length(result$IOVL)>0 && result$Iteration < li$maxiter){
              result$Iteration<-result$Iteration+1
              if(li$log){
                cat("############# beginning Iteration ",result$Iteration," #############\n\n")
              }
              
              #update WIJ
              
              #Berechne die Matrix W fuer alle i in IOVL und alle j
              #wie folgt: wij = max(ckj-cij,0)
              for(i in result$IOVL){ 
                #iteriere ueber alle Spalten
                for(j in 1:J){
                  #setze einsparung des Kunden gemaess max(0,...)
                  if(result$Iteration == 1){
                    result$w[i,j]<-max(result$cij[k,j]-result$cij[i,j],0) 
                  }else{
                    result$w[i,j]<-max(result$w[i,j]-result$w[k,j],0) 
                  } 
                } 
              } 
              
              #update wi/Wifi
              result$w$wi 	<- rowSums(result$w[ ,1:J])
              result$w$wifi 	<- result$w$wi - result$w$fi 
              
              if(li$log){
                
                cat("calculated W(ij)-Matrix:\n")
                print(result$w[result$IOVL, ])
              }
              
              # verwerfe alle laeger endgueltig, deren Gesamtersparnisse negativ sind
              IO.tmp <- result$IOVL[result$w$wifi[result$IOVL] <= 0]
              if(li$log){
                cat("closing the following warehouses eventually:", IO.tmp, "\n")
              }
              for(l in IO.tmp){
                result$IO <- c(result$IO, l)
                result$IOVL <- result$IOVL[-which(result$IOVL == l )] 
              } 
              if(li$log){
                cat("defined sets:\n")
                cat("\tIOVL: {", result$IOVL, "}\n")
                cat("\tI1:   {", result$I1, "}\n")
                cat("\tIO:   {", result$IO, "}\n") 
              }	
              
              if(length(result$IOVL) == 0){
                if(li$log){
                  cat("Add-Algorithm terminates because IOVL is empty.\n")
                }
                break # no further Storage found
              }
              
              #finde next lager
              k<- NA
              w.max <- 0 
              for(i in result$IOVL){
                if(result$w$wifi[i] > w.max){
                  w.max <- result$w$wifi[i]
                  k <- i
                }
              }
              if(is.na(k)) stop("This shouldn't happen. could not find cheapest warehouse.")
              #update I1 
              result$I1 <- c(result$I1, k)
              # Update IOVL
              result$IOVL <- result$IOVL[-which(result$IOVL== k )]
              #Update F
              result$totalcosts <-result$totalcosts - w.max
              
              #Update Entscheidungsvariable
              result$y[ k] <- TRUE
              result$x[ k,which(result$w[k,1:J] >0)] <- 1
              result$x[-k,which(result$w[k,1:J] >0)] <- 0
              object$warehouses$open[k] <- TRUE
               
              object$wlp$x <- result$x
              object$wlp$y <- result$y 
              object$wlp$F <- result$totalcosts
              object$wlp$I1 <- result$I1
              object$wlp$I1VL <- result$I1VL
              object$wlp$I0VL <- result$IOVL
              object$wlp$I0 <- result$IO 
              object$wlp$Iteration <- result$Iteration
              
              if(li$log){
                
                cat("chosen Warehouse: ", k, "(saving:",w.max,")\n")
                cat("current solution: ", result$totalcosts , "\n")
                cat("defined sets\n")		
                cat("\tIOVL: {", result$IOVL, "}\n")
                cat("\tI1:   {", result$I1, "}\n")
                cat("\tIO:   {", result$IO, "}\n")
                
                cat("Current Soultion x(ij)-Matrix:\n")
                print(result$x)
                
                cat("Current Soultion y(i):\n")
                print(result$y)
                
              }	
              
              if(length(result$IOVL) ==0) 
                break
              if(result$Iteration >= li$maxiter)
                break
            } 
            if(li$log){
              message("Target-Value:",result$totalcosts,"\n")
              message("This was(!) .OR.WLP.ADD")
            }
            return(object)
          }
)