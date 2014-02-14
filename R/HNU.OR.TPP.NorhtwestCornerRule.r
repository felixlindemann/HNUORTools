setGeneric("HNU.OR.TPP.NW",  function(object,...)  standardGeneric("HNU.OR.TPP.NW") )
 setMethod("HNU.OR.TPP.NW", signature(object="HNUGeoSituation"),
  function(object,...){ 
    
    li<-list(...)  
  	object <- HNU.OR.TPP.Prepare(object, ...) 			# repair degenerated if needed
	object <- HNU.OR.TPP.addTransportplan(object, ...)  # set initial Transportation Plan
	
	#set supply and demand
  	supply <- sapply(object$warehouses, function(o){o$supply})
	demand <- sapply(object$customers , function(o){o$demand})
	if(sum(supply) != sum(demand)) stop("This alg. can be used for non-degenerated solutions only: The Sums of warehouse$supply and customer$demand are not equal.")

	#set initial transportplan
	x <- object$transportplan * 0 
	

	for(i in (1:length(object$warehouses))){ 
		for(j in (1:length(object$customers))){
			x[i,j] <- min(c(supply[i],demand[j]))
			supply[i] <- supply[i] - x[i,j]
			demand[j] <- demand[j] - x[i,j] 
		}
	} 

	object$transportplan <- x  

	return(object)
	}
)