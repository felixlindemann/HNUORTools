setGeneric("HNU.OR.TPP.Prepare", function(object,...)  standardGeneric("HNU.OR.TPP.Prepare") )
 setMethod("HNU.OR.TPP.Prepare",signature(object="HNUGeoSituation"),
  function(object,...){  
    li<-list(...) 

    if(is.null(li$checkDegenerated)) li$checkDegenerated <- TRUE


    if(li$checkDegenerated){

    	supply <- sum(sapply(object$warehouses, function(o){o$supply}))
		demand <- sum(sapply(object$customers , function(o){o$demand}))


	    if(demand < supply){ 

			dummy <- new("HNUCustomer", x =0, y=0, demand = supply-demand, id="dummy", label="dummy")
			object$customers[[length(object$customers)+1]] <- dummy

			w<-paste("Less demand than supply - dummy customer added.")
			warning(w)
	    } else if( demand > supply){
			dummy <- new("HNUWarehouse", x =0, y=0, supply = demand-supply, id="dummy", label="dummy")
			object$warehouses[[length(object$warehouses)+1]] <- dummy
			w<-paste("Less supply than demand - dummy warehouse added.")
			warning(w)
		} 
	} 
	return (object)
  }
)
