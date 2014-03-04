setGeneric("HNU.OR.TSP.2OPT",  function(object,...)  standardGeneric("HNU.OR.TSP.2OPT") )
 setMethod("HNU.OR.TSP.2OPT", signature(object="HNUGeoSituation"),
  function(object,...){ 
    message("HNU.OR.TSP.2OPT\n")
  	li <- list(...) 

	found<-TRUE
	if(is.null(li$log))  li$log  <- FALSE
	if(is.null(li$plot)) li$plot <- FALSE
	
	R   <- object$tsp.solution$roundtrip 
	n   <- length(R)-1
	cij <- object$tsp.solution$cij
	locations <- sapply(object$tsp.solution$nodes, function(o) {o$label})

	if(li$log){
		cat("\taktuelle Route:\n")
		print(locations[R])
	}

	while(found){
		found <- FALSE
		for(i in 1:(n-2)){ 
			for(j in (i+2):(n)){ 


				vi<-c(R[i],R[i+1])
				vj<-c(R[j],R[j+1])
				if(vj[2] > n) vj[2] <- R[1]

				c.alt <- cij[vi[1],vi[2]] +cij[vj[1],vj[2]]
				c.neu <- cij[vi[1],vj[1]] +cij[vi[2],vj[2]]

				if(li$log == TRUE) {
					cat("Pruefe ob ", 
							locations[vi[1]], "-", 
							locations[vj[1]]," und ", 
							locations[vi[2]], "-", 
							locations[vj[2]]," kuerzer ist als ", 
							locations[vi[1]], "-", 
							locations[vi[2]]," und ", 
							locations[vj[1]], "-", 
							locations[vj[2]]," =  c.alt:",round(c.alt)," > c.neu: ",round(c.neu)," = ",c.alt>c.neu," \n") 
				}

				if(c.alt>c.neu){
					if(li$log == TRUE) {
						cat("Tausche ", 
								locations[vi[1]], "-", 
								locations[vj[1]]," und ", 
								locations[vi[2]], "-", 
								locations[vj[2]]," mit ", 
								locations[vi[1]], "-", 
								locations[vi[2]]," und ", 
								locations[vj[1]], "-", 
								locations[vj[2]],". Ersparnis: ",round(c.alt - c.neu),"\n") 
					}



					#neue Rundreise
					R <- c(R[1:i], R[j:(i+1)], R[(j+1):(n+1)])
					object$tsp.solution$roundtrip<-R
					# Neuer Zielfunktionswert
					
					x<-cij*0
					for(k in 2:length(R)){
						x[R[k-1],R[k] ]<-1
					}
					object$tsp.solution$F<-sum(cij*x) 
					object$tsp.solution$x<-x
					object$tsp.solution$roundtrip<-R

					if(li$log == TRUE){
						cat("\tNeue Route:\n")
						print(locations[R])
						if(length(R)!=n+1){
							cat("\n\n\n --------------------- ERRROR --------------------- \n---------------Neue Route zu lang --------------- \n\n")
							cat("i:",i," - j:",j,"\n")
							print(locations[R])
						}
					} 
					  
					found<-TRUE 
					break
				}
			}
			if(found == TRUE){
				break
			}
		}
	}

	return(object)
  }
 )