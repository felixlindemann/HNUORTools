#' @name TSP.NearestNeighbor 
#' @rdname TSP.NearestNeighbor 
#' @title Travelling-Salesman-Problem -- NearestNeighbor
#'
#' @description Improves a given Route by switching links.
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TSP.NearestNeighbor}}}{
#'    \describe{ 
#'    \item{log}{\code{logical} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}  
#'    \item{alpha}{\code{numeric} The \code{alpha}-Shape-Parameter of the savings-algorithm. Default is \code{1}}
#'    \item{calcCij}{\code{logical} \emph{Optional Parameter}. Indicating, if the cost matrix cij has to be calculated. Default is \code{TRUE}.}
#'    \item{cij}{\code{matrix} \emph{Optional Parameter}. Use for providing a user-defined cij-matrix. By default it's calculated.}
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{TPP.SteppingStone.GetPolygonZuege}}}
#'    }
#' }    
#' @keywords OR Travelling-Salesman-Problem TSP NearestNeighbor
#' @details Explain what NearestNeighbor does.
#' @return same modified object of Type \code{\link{GeoSituation}}.
#'      The Solution will be assigned the attribute \code{tsp.solution}.
#' @export  
#' @references Domschke
#' @seealso \code{\link{GeoSituation}}, \code{\link{Node}}, \code{\link{TSP.NearestNeighbor}}, \code{\link{TSP.2OPT}},  \code{\link{TSP.3OPT}}
#' @examples
#' # demo(HNUTSP01)
#' # demo(HNUTSP02) 
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
setGeneric("TSP.NearestNeighbor",  function(object,...)  standardGeneric("TSP.NearestNeighbor") )

#' @aliases TSP.NearestNeighbor,GeoSituation-method
#' @rdname TSP.NearestNeighbor
 setMethod("TSP.NearestNeighbor", signature(object="GeoSituation"),
  function(object,...){ 
    message("TSP.NearestNeighbor\n")
  	li <- list(...) 
  	# Voraussetzung: Die Kostenmatrix C;:; (cij) eines ungerichteten, vollstaendigen,schlichten,
	# bewerteten Graphen G = [V, E, c] mit n Knoten; ein Startknoten v1; 
	# Variable F fuer die Laenge der Rundreise.
	# 
	
	if(is.null(li$log)) li$log <- FALSE 
	if(is.null(li$StartNode)) li$StartNode <- 1 
	if(is.null(li$calcCij) | is.null(li$cij)) li$calcCij <- TRUE
 
	if(is.null(li$nodes)) stop("No Nodes for TSP are given.") 
	if(length(li$nodes) == 0) stop("The Origin-Datasource is empty") 
	object$tsp.nodes <- li$nodes

	if(li$calcCij){

		I <- length(object$tsp.nodes)
		J <- length(object$tsp.nodes)

		m<-matrix(rep(NA,I*J), nrow =I, ncol=J)
		 
		for(i in 1:I){
			n1<- object$tsp.nodes[[i]] 
			for(j in 1:J){
				n2<- object$tsp.nodes[[j]] 
				m[i,j] <- calc.Distance(n1,n2, ...) 
			}
		}  

		rownames(m) <- sapply(object$tsp.nodes, function(o){o$id})
		colnames(m) <- sapply(object$tsp.nodes , function(o){o$id})
	 
		object$tsp.costs <- m
	}else if(!is.null(li$cij)){
		object$tsp.costs <- li$cij
	}
	cij<- object$tsp.costs
	n<-length(object$tsp.nodes)
	if(is.null(cij)) stop("Error: No cost-matrix found")
	if(ncol(cij)!=n) stop("Unexpected Cost-Matrix. (Number of Columns != ",n,")")
	if(nrow(cij)!=n) stop("Unexpected Cost-Matrix. (Number of rows != "   ,n,")")
	x <-  matrix(rep(0,n*n),nrow=n)
	rownames(x) <- sapply(object$tsp.nodes, function(o){o$id})
	colnames(x) <- sapply(object$tsp.nodes, function(o){o$id})


	visitedNodes <- li$StartNode 

	cat("Using ",object$tsp.nodes[[li$StartNode]]$id,"as startnode.\n")

	iter <- 1
	F <- 0 # Target Value
	while(TRUE){
		# search next customer
		i <- visitedNodes[length(visitedNodes)]
		
		ubound <- max(cij[i,])* 3 +1
		lbound <- min(cij[i,])
		vj <- NA
		for (j in 1:n){
			if(length(which(visitedNodes == j)) ==0){
				# j not visited yet
				if(!is.na(cij[i,j])){
					if(cij[i,j]< ubound){
						vj <- j
						ubound <- cij[i,j]
						if (ubound == lbound) break # shorten this a little bit 
					}
				}
			}
		}
		if(is.na(vj)){
			if(length(visitedNodes) == n){	
				vj <- li$StartNode
			}else{
				stop("No nearest Neighbor could be found. please try different start node - as you may have used a condition like c[i,j] <- NA.")
			}
		} 
		visitedNodes <- c(visitedNodes, vj)
		F <- F + cij[i,vj]
		x[i,vj] <- 1
 
		object$tsp.solution <- list(
			iteration = iter, 
			F = F, 
			x = x, 
			roundtrip = visitedNodes,
			StartNode = li$StartNode,
			cij = object$tsp.costs,
			nodes = object$tsp.nodes
		)

		if(length(visitedNodes) == n+1) break # all customers are visited
		iter <- iter+1
	} 
	return(object)
 }
) 
 