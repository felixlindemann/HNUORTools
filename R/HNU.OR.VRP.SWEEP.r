setGeneric("HNU.OR.VRP.SWEEP",  function(object,...)  standardGeneric("HNU.OR.VRP.SWEEP") )
 setMethod("HNU.OR.VRP.SWEEP", signature(object="HNUGeoSituation"),
  function(object,...){ 
    message("HNU.OR.VRP.SWEEP\n")
  	li <- list(...) 

  	M <- length(object$warehouses)
  	N <- length(object$customers)
  	if(is.null(li$log)) li$log <- TRUE
  	if(is.null(li$round.cij)) li$round.cij <- TRUE
  	totalcosts <- 0
  	if(length(object$tpp.x) == 1){

  		if(M!=1)
  			stop("There is no Transportplan assigned. This is required as there are more than 1 warehouses.")

  		x <- matrix(sapply(object$customers, function(o){o$demand}), nrow=1, byrow=TRUE)

  		rownames(x) <- sapply(object$warehouses, function(o){o$id})
  		colnames(x) <- sapply(object$customers,  function(o){o$id})

  		object$tpp.x <- x

  	}else{

	  	if(nrow(object$tpp.x)!=M)
	  		stop("There is no valid Transportplan assigned. The number of warehouses is not equal to the number of rows in the transportation plan.")

	  	if(ncol(object$tpp.x)!=N)
	  		stop("There is no valid Transportplan assigned. The number of customers is not equal to the number of columns in the transportation plan.")

  	}

	if(!is.null(li$cij)){
		object$tsp.costs <- li$cij
	}
	
  	if(length(object$tsp.costs) <= 1){
 	
 		nodes <- list()

 		for ( i in 1:M){

 			nodes[[length(nodes)+1]] <- object$warehouses[[i]]

 		}

 		for ( j in 1:N){

 			nodes[[length(nodes)+1]] <- object$customers[[j]]

 		}

 		K <- length(nodes)

 		cij <- matrix(rep(0, K*K), ncol=K, nrow=K, byrow=TRUE)

 		for (i in 1:(K-1))
 		{
 			cij[i,i] <-0
 			n1 <- nodes[[i]]

 			for(j in (i+1):K){
 				n2 <- nodes[[j]]
 				cij[i,j] <- calc.Distance(n1,n2,...)
 				cij[j,i] <- cij[i,j]
 			}
 			
 		}
 		if(li$round.cij)
 			cij <- round(cij)
 		object$tsp.nodes <- nodes
 		object$tsp.costs <- cij
 		if(li$log){
 			warning("\nCij has been calculated.\nNodes have been assigned to TSP.\n") 
 		}
 		
  	}else{	

	  	if(nrow(object$tsp.costs)!=M)
	  		stop("There is no valid costmatrix assigned. The number of warehouses is not equal to the number of rows in the transportation plan.")

	  	if(ncol(object$tsp.costs)!=N)
	  		stop("There is no valid costmatrix assigned. The number of customers is not equal to the number of columns in the transportation plan.")

  	}
 	
 
  	if(is.null(li$vehiclecapacity.maxstops)) 
	{	
		li$vehiclecapacity.maxstops <- N*2+1
		warning("Maximum number of stops is not given. Constraint not taken into account.")
	}
  	if(is.null(li$vehiclecapacity)) 
	{	
		li$vehiclecapacity <- sum(object$tpp.x)*2+1
		warning("Maximum Vehicle Loading Capacity is not given. Constraint not taken into account.")
	} 
	if(is.null(li$rotateClockwise)) li$rotateClockwise <- FALSE



  	for(i in 1:M){
  		# iterate over all warehouses
  		w<- object$warehouses[[i]]
  		if(li$log){
  			cat("Analyzing warehouse:",w$label,"\n")
  		}
  		df<-data.frame()
  		for(j in 1:N){ 
  			if(object$tpp.x[i,j] >0){
  				cust <- object$customers[[j]] 
  				df<-rbind(df, data.frame(j = j , polar= calc.polar(w,cust, ...) ))
  			} 
  		} 
  		# sort by col2.
  		if(li$rotateClockwise ){
  			df <- df[ order( -df[,2]), ]
		} else{
			df <- df[ order( df[,2]), ]
		} 
		vrp <- list()
		vrp$customers<-df
		vrp$maxstops <- li$vehiclecapacity.maxstops 
		vrp$maxcapacity <- li$vehiclecapacity
		vrp$x   <- object$tsp.costs * 0
		vrp$cij <- object$tsp.costs

		vrp$tours <- list()

		tour <- NULL
  		 
		vi <- i
  		for( j in 1:nrow(df)){

			vj <- df$j[j] + M
  			cust <- object$customers[[df$j[j] ]]
  			NT <- FALSE 
  			if(is.null(tour)){
  				NT <- TRUE 
  			} else{ 
				# checking loading capcity
				if(tour$loading + cust$demand > li$vehiclecapacity){
					NT <- TRUE
					cat("\t\t Maximum loading capacity exceeded, if next customer (",cust$label,"demand:",cust$demand ,") will be added (",tour$loading ,"+", cust$demand ,">", li$vehiclecapacity,").",
						"\n\t\t I am Going to end the current Tour.\n")
				} else{
					# checking maximum number of stops
					if(length(tour$stops) >=li$vehiclecapacity.maxstops){
						NT <- TRUE
						cat("\t\t Maximum number of stops exceeded, if next customer (",cust$label,") will be added (",length(tour$stops) ,">=",li$vehiclecapacity.maxstops,").",
							"\n\t\t I am Going to end the current Tour.\n")
					}
				} 
  			} 
  			if(NT){	
  				NT <- FALSE
  				if(!is.null(tour)){
  					# Add Warehouse as last stop
	  				vrp$x[vi,i] <- 1
	  				tour$costs <- tour$costs + 
	  					vrp$cij[vi, i] 
	  				# store old Tour
  					vrp$tours[[length(vrp$tours)+1]] <- tour   
  					if(li$log){
						cat("\t\t Tourcosts:",tour$costs  ,"\n")
					}  	
  				}
  				if(li$log) cat("\tTour #",length(vrp$tours)+1,":\n") 
  				tour <- list()
		  		tour$loading <- 0
		  		tour$costs <- 0	
		  		tour$stops <-0
		  		tour$stops.list    <- list()
		  		tour$stops.indices <- list()
		  		vi <- i
			}	

  			if(li$log){
  				cat("\t\t",cust$label, "(demand:",cust$demand,", angle:",df$polar[j],")","\n")
  			}
			tour$loading <- tour$loading + cust$demand
			tour$stops <- tour$stops + 1
			tour$stops.list[[length(tour$stops.list)+1]] <- cust
			tour$stops.indices[[length(tour$stops.indices)+1]] <- vj

			tour$costs <- tour$costs + object$tsp.costs[vi,vj]
			vrp$x[vi,vj]<- 1
			vi <- vj
  		}
  		if(!is.null(tour)){
  			if(tour$stops > 0){
  				# Add Warehouse as last stop
  				vrp$x[tour$stops.indices[[length(tour$stops.indices)]],i] <- 1
  				tour$costs <- tour$costs + 
  					vrp$cij[
  						tour$stops.indices[[length(tour$stops.indices)]],
  						i
  					] 
  				# store old Tour
					vrp$tours[[length(vrp$tours)+1]] <- tour  	
				if(li$log){
					cat("\t\t No customers left. \n\t\t I am Going to end the current Tour.\n")
					cat("\t\t Tourcosts:",tour$costs  ,"\n")
				}  					
  				 
  			}
  		} 
  		vrp$totalcosts <- sum(object$tsp.costs*vrp$x)
  		totalcosts <- totalcosts + vrp$totalcosts
		if(li$log){
			cat("\tTotalcosts for ",w$label,":",vrp$totalcosts ,"\n")
		}
		w$vrp <- vrp
		object$warehouses[[i]] <- w
  	}
  	if(li$log){
  		message(paste("Total costs are:", totalcosts))
  	}
  	return(object)
})
