setGeneric("HNU.OR.TSP.drawrouting",  function(object,...)  standardGeneric("HNU.OR.TSP.drawrouting") )
 setMethod("HNU.OR.TSP.drawrouting", signature(object="HNUGeoSituation"),
  function(object,...){ 
    message("HNU.OR.TSP.drawrouting\n")
  	li <- list(...) 

  	n<- length(object$tsp.nodes)
  	if(n <=1) stop("This is not a TSP.")
  	if(nrow(object$tsp.solution$x) != n) stop("Error. TSP not correctly initalized")

  	s.node <- object$tsp.nodes[[object$tsp.solution$StartNode ]]
	points(s.node$x, s.node$y, cex=3, pch=20, col=2)
	points(s.node$x, s.node$y, cex=1, pch=20, col=1)
  	for(i in 1:n){
  		n1<- object$tsp.nodes[[i]]

  		for(j in 1:n){

  			if(object$tsp.solution$x[i,j] == 1){

  				n2<-object$tsp.nodes[[j]]

  				arrows(n1$x, n1$y, x1 = n2$x, y1 = n2$y, 
  					length = 0.25, angle = 30, code = 2,  ...) 
  			} 
  		} 
  	} 
  }
)
 