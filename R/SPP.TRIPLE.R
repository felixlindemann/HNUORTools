#' @name SPP.TRIPLE
#' @rdname SPP.TRIPLE
#' @title Shortest-Path-Problem -- TRIPLE Algorithm of Floyd-Warshall
#'
#' @description Calculates the Shortest Paths in a network.
#' @details This implementation uses the \code{costs} attribute of the \code{\link{Link}s} provided.
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... See below for optional parameters.
#' @section Optional Parameters (\code{...}):  
#' \subsection{used by SPP.TRIPLE}{
#'    \describe{ 
#'     \item{\code{log}}{
#'			 \emph{optional} \code{"logical"}. Indicating if the calculations should be logged. \strong{Default} is \code{FALSE}.}
#'   	\item{\code{debug}}{
#'			 \emph{optional} \code{"logical"}. Indicating if the calculations should be debugged \strong{Default} is \code{FALSE}. This means, that each process step is logged to the console.}
#'		\item{\code{start}}{
#'			 \emph{optional} \code{"numeric"}. Indicates which \code{\link{Node}}(-index) should be used as startnode. 
#'			  \strong{Default} is 1. 
#'
#'			  \strong{Has to be a positive 0 < value <= N} (with N = the number of nodes in the Scenario.)}
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{	
#'    \itemize{
#'		\item{\code{...} is currently not forwared.}
#'    }
#' } 
#' @keywords OR Shortest-Path-Problem SPP Dijkstra
#' @export  
#' @references Domschke, Wolfgang; Drexl, Andreas (2005): Einfuehrung in Operations Research. Mit 63 Tabellen. 6., ueberarb. und erw. Aufl. Berlin: Springer.

#' @return same modified object of Type \code{\link{GeoSituation}}.
#'      The Solution will be assigned the attribute \code{shortestpath}.
#' @seealso \code{\link{GeoSituation}}, \code{\link{Node}}  
#' @examples
#' # demo(HNUSPP01)
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
setGeneric("SPP.TRIPLE",  function(object,...)  standardGeneric("SPP.TRIPLE") )
#' @aliases SPP.TRIPLE,GeoSituation-method
#' @rdname SPP.TRIPLE
setMethod("SPP.TRIPLE", signature(object="GeoSituation"),
          function(object,...){ 
            message("executing SPP.TRIPLE ...")
            li <- list(...)
            if(is.null(li$log))  li$log <- FALSE 
            if(is.null(li$debug))  li$debug <- FALSE 
            if(is.null(li$start)) li$start <- 1 
            
            nodes <- object$nodes 
            N <- length(nodes)
            L <- length(object$links)
            if(L == 0) { stop("Dijkstra not solvable: No links found!")} 
            
            if((li$start) <=0) stop("Dijkstra not solvable: The Parameter 'start' doesn't have a positive Value. The parameter 'start' is the index for the start-node.")
            
            if(!(li$start) <=N) stop("Dijkstra not solvable: The Parameter 'start' has a value which is larger than the number of Nodes in the provided network.")
            cij <- matrix(rep(NA, N^2), ncol=N, byrow = TRUE)  
            VG  <- matrix(rep(0 , N^2), ncol=N, byrow = TRUE)  
            DG  <- matrix(rep(NA, N^2), ncol=N, byrow = TRUE)  
            RG  <- matrix(rep(0 , N^2), ncol=N, byrow = TRUE)  
            # Log
            if(li$debug) cat("\tEstablishing cij matrix by given network (links)")
            
            # check links and create cij matrix
            for(l in 1:L){  
              
              link <- object$links[[l]]
              
              if(link$costs < 0) { stop("Dijkstra not solvable: negative costs for link are not supported")}
              link$used <- FALSE 	# init link not used
              
              origin <- link$origin
              destination <- link$destination
              
              for(i in 1:N){
                cij[i,i] <- 0
                VG[i,i]<-i
                if(origin$id == nodes$id[i]){
                  for(j in 1:N){ 
                    if(destination$id == nodes$id[j]){
                      value <- link$costs
                      cij[i,j] <- value
                      VG[i,j]<-i
                      if(link$oneway == FALSE){
                        cij[j,i] <- value
                        VG[j,i]<-j
                      }
                      break
                    }
                  }	
                  break
                }
              } 
            }  
            colnames(cij) <- object$nodes$id
            rownames(cij) <- object$nodes$id
            # Log
            if(li$log) cat("\tcij used for Dijkstra:\n")
            if(li$log) print(cij)
            DG<-cij
            RG<-VG
            
            
            iter<-1  
            
            for(j in 1:N){
              for(i in 1:N){
                for(k in 1:N){
                  su <- DG[i,j] + DG[j,k] 
                  if(is.na(su )){
                    #do nothing
                  }else { 
                    doit<-FALSE
                    if(is.na(DG[i,k])) {
                      doit<-TRUE
                    }else{
                      if(su < DG[i,k]){
                        doit <- TRUE
                      }
                    }
                    if(doit){
                      DG[i,k]<-su
                      RG[i,k] <- RG[j,k]
                    }
                  }
                }
              }
            }
            object$spp$cij <- cij
            object$spp$VG <- VG
            object$spp$DG <- DG
            object$spp$RG <- RG 
            # Log
            if(li$log) {
              cat("Final solution:\n")
              print(object$spp) 
            }
            message("SPP.TRIPLE done.")
            return (object)
          })

