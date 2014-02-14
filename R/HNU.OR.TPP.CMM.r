setGeneric("HNU.OR.TPP.CMM",  function(object,...)  standardGeneric("HNU.OR.TPP.CMM") )
 setMethod("HNU.OR.TPP.CMM", signature(object="HNUGeoSituation"),
  function(object,...){ 
    
    li<-list(...)  
  	object <- HNU.OR.TPP.Prepare(object, ...) 			# repair degenerated if needed
	object <- HNU.OR.TPP.addTransportplan(object, ...)  # set initial Transportation Plan
	object <- HNU.OR.TPP.addTransportCosts(object, ...) # make sure, Transportcostsmatrix (cij) is set.
    cij    <- object$transportcosts 					# store transportcosts localy

	I <- length(object$warehouses)
	J <- length(object$customers)
	
	#set supply and demand
  	supply <- sapply(object$warehouses, function(o){o$supply})
	demand <- sapply(object$customers , function(o){o$demand})

	#set initial transportplan
	x <- object$transportplan * 0 

	for(j in (1:J)){ 
		while(demand[j]>0){		
			i<-NA
			c.min<- 100000000000000000	
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
	}  

	object$transportplan <- x   
	return(object)
  }
)