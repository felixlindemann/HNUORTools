

setGeneric("HNU.OR.getCostMatrix", function(object,...)  standardGeneric("HNU.OR.getCostMatrix") )
 setMethod("HNU.OR.getCostMatrix",signature(object="HNUGeoSituation"),
  function(object,...){  
    li<-list(...) 


	I <- length(object$warehouses)
	J <- length(object$customers)
	if(is.null(li$transportcosts)) li$transportcosts <- 1
	demand <- sapply(object$customers , function(o){o$demand})
	cij <- li$cij		
	if(is.null(cij)){	
		cij <- object$tpp.costs

		if(
			length(cij) == 1 
			| nrow(cij)	!= I 
			| ncol(cij)	!= J 
			| sum(is.na(cij))>0
		){ 


			cij <- HNU.OR.getDistanceMatrix(object,"warehouses", "customers", ...)*li$transportcosts 
     		
		}  
	}  
	if(class(cij) != "matrix")   stop(paste("The object 'cij' is of type",class(cij),"- expected is matrix."))
	if(nrow(cij)!= I) 			stop(paste("The number of rows of object 'cij' is", nrow(cij),"- expected was", I))
	if(ncol(cij)!= J) 			stop(paste("The number of cols of object 'cij' is", ncol(cij),"- expected was", J))
	if(min(cij)<0) 				stop("Negative Values are not permitted in cij.")
	if(sum(is.na(cij))>0)		stop("NAs are not permitted in cij.") 
	 

	return (cij)
  }
)
