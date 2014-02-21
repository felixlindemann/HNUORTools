setGeneric("HNU.OR.TPP.MMM",  function(object,...)  standardGeneric("HNU.OR.TPP.MMM") )
 setMethod("HNU.OR.TPP.MMM", signature(object="HNUGeoSituation"),
  function(object,...){ 
    
    li<-list(...)  
  	object <- HNU.OR.TPP.Prepare(object, ...) 			# repair degenerated if needed
	cij    <- HNU.OR.TPP.getCostMatrix(object, ...)		# store transportcosts localy
	object$tpp.costs <- cij
	
	I <- length(object$warehouses)
	J <- length(object$customers)
	
	#set supply and demand
  	supply <- sapply(object$warehouses, function(o){o$supply})
	demand <- sapply(object$customers , function(o){o$demand})

	if(sum(supply) != sum(demand)) stop("This alg. can be used for non-degenerated solutions only: The Sums of warehouse$supply and customer$demand are not equal.")

	#set initial transportplan
	x <- HNU.OR.getInitialMatrix(object, ...)  # set initial Transportation Plan
	
   	while(sum(demand)>0){	

   		ij <- NULL
		c.min<- max(cij) * 2 +1	
		for(i in 1:I){
			if(supply[i]>0){
				for(j in 1:J){
					if(demand[j]> 0){
						if(cij[i,j]<c.min) {
							ij <- c(i,j)
							c.min <- cij[i,j]
						}
					}
				}
			}
		}

		if(!is.null(ij)){
			i<- ij[1]
			j<- ij[2]
			x[i,j] <- min(c(supply[i],demand[j]))
			supply[i] <- supply[i] - x[i,j]
			demand[j] <- demand[j] - x[i,j] 
		}else{
			stop("this should not happen.")
		}
		if(sum(demand) + sum(supply) == 0) break
   	}
 
	object$tpp.x <- x   
	return(object)
  }
)