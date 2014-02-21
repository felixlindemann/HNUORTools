setGeneric("HNU.OR.TPP.CMM",  function(object,...)  standardGeneric("HNU.OR.TPP.CMM") )
 setMethod("HNU.OR.TPP.CMM", signature(object="HNUGeoSituation"),
  function(object,...){ 
    
    li<-list(...)  
  	object  <- HNU.OR.TPP.Prepare(object, ...)			# repair degenerated if needed
	cij 	<- HNU.OR.TPP.getCostMatrix(object, ...)	# store transportcosts localy
	object$tpp.costs <- cij
	
	I <- length(object$warehouses)
	J <- length(object$customers)
	
	#set supply and demand
  	supply <- sapply(object$warehouses, function(o){o$supply})
	demand <- sapply(object$customers , function(o){o$demand})

	if(sum(supply) != sum(demand)) stop("This alg. can be used for non-degenerated solutions only: The Sums of warehouse$supply and customer$demand are not equal.")

	#set initial transportplan
	x <- HNU.OR.getInitialMatrix(object, ...)  # set initial Transportation Plan
	

	if(is.null(li$domschke.version)) li$domschke.version <- 2007

	if(li$domschke.version == 1995){
		message("Using Domschke 1995\n")
		while(sum(demand)>0){		 
			for(j in (1:J)){ 
				if(demand[j]>0){
					i<-NA
					c.min<- max(cij) * 2 +1	
					for(k in (1:I)){
						if(supply[k]>0){					
							if(cij[k,j]<c.min) {
								i <- k
								c.min <- cij[k,j]
							}
						}
					}
					if(!is.na(i)){
						x[i,j] <- min(c(supply[i],demand[j]))
						supply[i] <- supply[i] - x[i,j]
						demand[j] <- demand[j] - x[i,j] 
					}else{
						stop("this should not happen.")
					}
				}
				if(sum(demand) + sum(supply) == 0) break
			}
		}   
	}else{
		message("Using Domschke 2007\n")

		for(j in (1:J)){ 
			while(demand[j]>0){		
				i<-NA
				c.min<- max(cij) * 2 +1	
				for(k in (1:I)){
					if(supply[k]>0){					
						if(cij[k,j]<c.min) {
							i <- k
							c.min <- cij[k,j]
						}
					}
				}
				if(!is.na(i)){
					x[i,j] <- min(c(supply[i],demand[j]))
					supply[i] <- supply[i] - x[i,j]
					demand[j] <- demand[j] - x[i,j] 
				}else{
					stop("this should not happen.")
				}
			}
			if(sum(demand) + sum(supply) == 0) break
		}   
	}
	object$tpp.x <- x   
	return(object)
  }
)