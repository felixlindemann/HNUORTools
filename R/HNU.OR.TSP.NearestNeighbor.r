setGeneric("HNU.OR.TSP.NearestNeighbor",  function(object,...)  standardGeneric("HNU.OR.TSP.NearestNeighbor") )
 setMethod("HNU.OR.TSP.NearestNeighbor", signature(object="HNUGeoSituation"),
  function(object,...){ 
    message("HNU.OR.TSP.NearestNeighbor\n")
  	li <- list(...) 
  	# Voraussetzung: Die Kostenmatrix C;:; (cij) eines ungerichteten, vollstaendigen,schlichten,
	# bewerteten Graphen G = [V, E, c] mit n Knoten; ein Startknoten v1; 
	# Variable F fuer die Laenge der Rundreise.
	# 
	if(is.null(li$useNodes))  li$useNodes  <- "nodes"
	if(is.null(li$StartNode)) li$StartNode <- 1
	if(is.null(li$calcCij) & is.null(li$cij)) li$calcCij <- TRUE


	if(li$useNodes == "customers"){
		nodes <- object$customers
	}else{
		nodes <- object$nodes
	}
	object$tsp.nodes <- nodes

	if(li$calcCij){
		object$tsp.costs <- HNU.OR.getDistanceMatrix(object, li$useNodes, li$useNodes, ...)
	}else if(!is.null(li$cij)){
		object$tsp.costs <- li$cij
	}
	cij<- object$tsp.costs
	n<-length(nodes)
	if(is.null(cij)) stop("Error: No cost-matrix found")
	if(ncol(cij)!=n) stop("Unexpected Cost-Matrix. (Number of Columns != ",n,")")
	if(nrow(cij)!=n) stop("Unexpected Cost-Matrix. (Number of rows != "   ,n,")")
	x <-  matrix(rep(0,n*n),nrow=n)
	rownames(x) <- sapply(nodes, function(o){o$id})
	colnames(x) <- sapply(nodes, function(o){o$id})


	visitedNodes <- li$StartNode 
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
			StartNode = li$StartNode
		)

		if(length(visitedNodes) == n+1) break # all customers are visited
		iter <- iter+1
	} 
	return(object)
 }
) 
 