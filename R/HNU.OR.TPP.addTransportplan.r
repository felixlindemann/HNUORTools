setGeneric("HNU.OR.TPP.addTransportplan", function(object,...)  standardGeneric("HNU.OR.TPP.addTransportplan") )
 setMethod("HNU.OR.TPP.addTransportplan",signature(object="HNUGeoSituation"),
  function(object,...){  
    li<-list(...) 

   
	I <- length(object$warehouses)
	J <- length(object$customers)

	x <- matrix(rep(0, I*J), ncol = J, byrow = TRUE)

	rownames(x) <- sapply(object$warehouses, function(o){o$id})
	colnames(x) <- sapply(object$customers , function(o){o$id})

	object$transportplan <- x  
 
	return (object)
  }
)
