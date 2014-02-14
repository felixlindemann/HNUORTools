setGeneric("HNU.OR.TPP.addTransportCosts", function(object,...)  standardGeneric("HNU.OR.TPP.addTransportCosts") )
 setMethod("HNU.OR.TPP.addTransportCosts",signature(object="HNUGeoSituation"),
  function(object,...){  
    li<-list(...) 
	I <- length(object$warehouses)
	J <- length(object$customers)
	if(is.null(li$transportcosts)) li$transportcosts <- 1
	demand <- sapply(object$customers , function(o){o$demand})
			
	if(is.null(li$cij)){	
		cij <- object$transportcosts

		if(
			length(cij) == 1 
			| nrow(cij)	!= I 
			| ncol(cij)	!= J 
			| sum(is.na(cij))>0
		){ 


			cij <- getDistanceMatrix(object,"warehouses", "customers", ...)*li$transportcosts
     		
			

			# li$cij is now a distance matrix/cost matrix (if transportcosts are provided). 
			# 
			# in this context we need the costs per transported item.
			# so we calculate for each pivot-element cij[i,j] <- cij[i,j] * demand[j]
			cij <- t(t(cij)*demand) 

		} 
		li$cij <- cij
	} 

	if(class(li$cij) != "matrix")   stop(paste("The object 'cij' is of type",class(li$cij),"- expected is matrix."))
	if(nrow(li$cij)!= I) 			stop(paste("The number of rows of object 'cij' is", nrow(li$cij),"- expected was", I))
	if(ncol(li$cij)!= J) 			stop(paste("The number of cols of object 'cij' is", ncol(li$cij),"- expected was", J))
	if(min(li$cij)<0) 				stop("Negative Values are not permitted in cij.")
	if(sum(is.na(li$cij))>0)		stop("NAs are not permitted in cij.") 
	object$transportcosts <- li$cij

	return (object)
  }
)
