setGeneric("HNU.OR.getInitialMatrix", function(object,...)  standardGeneric("HNU.OR.getInitialMatrix") )
 setMethod("HNU.OR.getInitialMatrix",signature(object="HNUGeoSituation"),
  function(object,...){  
    li<-list(...) 

   	if(is.null(li$initialvalue))  li$initialvalue <- 0

	I <- length(object$warehouses)
	J <- length(object$customers)

	x <- matrix(rep( li$initialvalue , I*J), ncol = J, byrow = TRUE)

	rownames(x) <- sapply(object$warehouses, function(o){o$id})
	colnames(x) <- sapply(object$customers , function(o){o$id})
 
 
	return (x)
  }
)
