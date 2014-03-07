#' @name SPP.Dijkstra
#' @rdname SPP.Dijkstra
#' @title Shortest-Path-Problem -- Algorithm of Dijkstra
#'
#' @description Calculates the Shortest Path from one node to all others in a network.
#' @details This implementation uses the \code{costs} attribute of the \code{\link{Link}s} provided.
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... See below for optional parameters.
#' @section Optional Parameters (\code{...}):	
#' \subsection{used by SPP.Dijkstra}{
#'    \describe{ 
#' 		\item{\code{log}}{
#'			 \emph{optional} \code{"logical"}. Indicating if the calculations should be logged. \strong{Default} is \code{FALSE}}
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
#' @references Domschke
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
setGeneric("SPP.Dijkstra",  function(object,...)  standardGeneric("SPP.Dijkstra") )
#' @aliases SPP.Dijkstra,GeoSituation-method
#' @rdname SPP.Dijkstra
 setMethod("SPP.Dijkstra", signature(object="GeoSituation"),
  function(object,...){ 
    cat("SPP.Dijkstra\n")
  	li <- list(...)
 	if(is.null(li$log))  li$log <- FALSE 
 	if(is.null(li$start)) li$start <- 1 

 	nodes <- object$nodes 
	N <- length(nodes)
 	L <- length(object$links)
 	

 	if(!(li$start) >0) stop("Dijkstra not solvable: The Parameter 'start' doesn't have a positive Value.")


 	if(!(li$start) <=N) stop("Dijkstra not solvable: The Parameter 'start' has a value which is larger than the number of Nodes in the provided network.")
 	cij <- matrix(rep(NA, N^2), ncol=N, byrow = TRUE) 
 	 

 	if(L == 0) { stop("Dijkstra not solvable: No links found")}
 	# Log
 	if(li$log) cat("\tEstablishing cij matrix by given network (links)")

 	# check links and create cij matrix
 	for(l in 1:L){ 
 		link <- object$links[[l]]
 		if(link$costs < 0) { stop("Dijkstra not solvable: negative costs for link are not supported")}
 		link$used <- FALSE 	# init link not used
 		object$links[[l]] <- link

 		origin <- link$origin
 		destination <- link$destination
 		
 		for(i in 1:N){
 			if(origin$id == nodes[[i]]$id){
 				for(j in 1:N){
 					if(destination$id == nodes[[j]]$id){
 						value <- link$costs
 						cij[i,j] <- value
 						if(link$oneway == FALSE){
 							cij[j,i] <- value
 						}
 						break
 					}
 				}	
 				break
 			}
 		} 
 	} 

 	colnames(cij) <- sapply(object$nodes, function(o){o$id})
 	rownames(cij) <- sapply(object$nodes, function(o){o$id})
 	# Log
 	if(li$log) cat("\tcij:\n")
 	if(li$log) print(cij)

 	m<- matrix(rep(c(NA,NA), N), ncol=2, byrow = TRUE)
 	m<- data.frame(m)
 	tableau <- NULL
 	rownames(m) <- sapply(object$nodes, function(o){o$id})
 	colnames(m) <- c("d","p")
 	
 	iter<-0
 	Q 	<- data.frame(i = li$start, d= 0, p= 0) 
 	if(is.null(li$stopafter)) li$stopafter <- N
 	
	object$shortestpath <- list(iteration = iter, Q = Q, tableau= m)
 	while (iter<=li$stopafter & length(Q) > 0){ # max N Iterations
 		object$shortestpath$iter <- iter
 		# get current Q
 		if(li$log) cat("\n\n----------- new iteration:", iter, "-----------\n\n")
		i <- Q[1,"i"]
		d <- Q[1,"d"]
		p <- Q[1,"p"] 
		# Log
 		if(li$log) {
 			cat("\t\tCurrent Solution (sorted Q):", rownames(m)[Q$i],"\n")
 		}

		# Log
 		if(li$log) {
 			cat("\t\t\tchoosing and removing node:",rownames(m)[i], "(d:", d, "/p:", p,") from Q:\n") 
 		}
 		if(p > 0){
 			origin 		<- object$nodes[[p]]
 			destination <- object$nodes[[i]] 
 			for(l in 1:L){
 				link <- object$links[[l]]
 				if((link$origin$id == origin$id      & link$destination$id == destination$id)|
 				   (link$origin$id == destination$id & link$destination$id == origin$id & link$oneway == FALSE)){
 					link$used <- TRUE 
 					object$links[[l]] <- link
 					break	
 				}
 			} 
 		}

 		Q<-Q[-1,] #remove first element of Q
		#update result.
 		m[i,"d"] <- d
		m[i,"p"] <- p
 		
 		if(is.null(tableau)){
 			tableau <- m
 		}else{
			tableau <- cbind(tableau,m)
		}
 		p<-i  
 		#check for new connections
 		for(j in 1:N){
 			if(j != p){ #exclude current source node
 				k<- cij[p,j]
 				if(!is.na(k)){
 					n.dist <- d +k
	 				if( is.na( m[j,"d"] ) ){
	 					# node j can be reached first time 
	 					df <- data.frame(i = j, d = n.dist, p=p)
	 					rownames(df) <- object$nodes[[j]]$id
	 					Q  <- rbind(Q, df)
	 					m[j,"d"] <- n.dist
	 					m[j,"p"] <- p
 						if(li$log) cat("\t\t\tnew connection (",rownames(m)[p],"/",rownames(m)[j], ").",rownames(m)[j], "added to Q. dist:",n.dist,"\n")

	 				}else if(m[j,"d"] > n.dist){
	 					# update node
	 					# TODO: check if j is in Q
	 					m[j,"d"] <- n.dist
	 					m[j,"p"] <- p 
 						if(li$log) cat("\t\t\tbetter connection (",rownames(m)[p],"/",rownames(m)[j], ").",rownames(m)[j], "added to Q. dist:",n.dist,"\n")
					} else {
	 					# do nothing
	 				} 
	 			} 
 			}
 		}

 		# sort Q
 		Q<-Q[ order(Q[,"d"]), ] 
		object$shortestpath <- list(iteration = iter, Q = Q, tableau= m)
		

 		if(nrow(Q) == 0) break
 		iter <- iter + 1
 	}
 	object$shortestpath$finaltableau <- tableau
	 
 	# Log
	if(li$log) {
		cat("Final solution (sorted Q):\n")
		print(object$shortestpath$finaltableau) 
	}

 	return (object)
})

