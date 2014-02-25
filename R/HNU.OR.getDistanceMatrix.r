
setGeneric("HNU.OR.getDistanceMatrix",  function(object,...)  standardGeneric("HNU.OR.getDistanceMatrix") )

setMethod("HNU.OR.getDistanceMatrix",signature(object="HNUGeoSituation"),
	function(object,sorigin,sdestination,...){
		li<-list(...) 
		fields<- c( "nodes", "customers", "warehouses")

		f<-agrep(sorigin, fields, max = 1, ignore.case = TRUE)
		if(length(f)!=1){stop("The origin-Data-Source could not be identified")}
		t<-agrep(sdestination, fields, max = 1, ignore.case = TRUE)
		if(length(t)!=1){stop("The To-Data-source could not be identified")}
		 

		origin 		<- list()
		destination <- list()

		if(f == 1){
			origin<- object@nodes
		}else if(f == 2){
			origin<- object@customers
		}else if(f == 3){
			origin<- object@warehouses
		}else{
			stop("The origin-Data-Source could not be identified")
		}
		if(t == 1){
			destination<- object@nodes
		}else if(t == 2){
			destination<- object@customers
		}else if(t == 3){
			destination<- object@warehouses
		}else{
			stop("The Destination-Data-Source could not be identified")
		}
		
		if(length(origin) == 0){
			stop("The Origin-Datasource is empty")
		}
		if(length(destination) == 0){
			stop("The destination-Datasource is empty")
		}

		I <- length(origin)
		J <- length(destination)

		m<-matrix(rep(NA,I*J), nrow =I, ncol=J)
		 
		for(i in 1:I){
			n1<- origin[[i]] 
			for(j in 1:J){
				n2<- destination[[j]] 
				m[i,j] <- calc.Distance(n1,n2, ...) 
			}
		}  

		rownames(m) <- sapply(origin, function(o){o$id})
		colnames(m) <- sapply(destination , function(o){o$id})
  
	    return(m)
	}
)