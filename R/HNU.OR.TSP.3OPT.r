setGeneric("HNU.OR.TSP.3OPT",  function(object,...)  standardGeneric("HNU.OR.TSP.3OPT") )
 setMethod("HNU.OR.TSP.3OPT", signature(object="HNUGeoSituation"),
  function(object,...){ 
    message("HNU.OR.TSP.3OPT\n")
  	li <- list(...) 

	found<-TRUE
	if(is.null(li$log))  li$log  <- FALSE
	if(is.null(li$plot)) li$plot <- FALSE
	
	R   <- object$tsp.solution$roundtrip 
	n   <- length(R)-1
	cij <- object$tsp.solution$cij
	if(li$log){
		cat("\tNeue Route:\n")
		print(R)
	}
	while(found){
		found <- FALSE 
		for(zaehler in 1:n){
			for(h in 1:(n-3)){
				for(j in (h+1):(n-1)){

					d <- cij[ R[n],R[1] ] + cij[ R[h],R[h+1]] + cij[ R[j],R[j+1]]

					d1<- cij[ R[h],R[j+1] ]+ cij[ R[1],R[j] ]
					d2<- cij[ R[1],R[j+1] ] + cij[ R[h],R[j] ]

					T<-matrix(
						c(R[n], R[1], R[h],R[h+1],R[j],R[j+1]),
						ncol=2,byrow=TRUE)

					if(d1<=d2){ 

						if(d1 + cij[ R[h+1],R[n] ]<d){
							found<-TRUE
							R <- c(R[1:h],R[(j+1):n],R[(h+1):j],R[1])
							Variante<-1
						}
					}else{
						if(d2 + cij[ R[h+1],R[n] ]<d){
							found <- TRUE
							R<- c(R[1],R[(j+1):n], R[(h+1):j], R[h:1])
							Variante<-2
						}
						
					} 
					if(found==TRUE){
						break
					}
				}
				if(found==TRUE){
					x<-cij*0
					for(k in 2:length(R)){
						x[R[k-1],R[k] ]<-1
					}
					object$tsp.solution$F<-sum(cij*x) 
					object$tsp.solution$x<-x
					object$tsp.solution$roundtrip<-R
					break
				}else{
					R<-c(R[n], R[1:(n-1)],R[n]) 
				}
			} 
			if(found==TRUE){
				break
			}
		}  
	}
	return(object)
  }
 )