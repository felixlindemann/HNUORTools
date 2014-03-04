setGeneric("plotGeoSituation.transportplan", function(object,...)  standardGeneric("plotGeoSituation.transportplan") ) 
 setMethod("plotGeoSituation.transportplan",signature(object="HNUGeoSituation"),
  function(object,...){
    li<-list(...)  
    x<- NA
    if(is.null(li$isWLP)) li$isWLP <- FALSE
		
	if(li$isWLP)
		x <- object$wlp.solution$x		# store transportplan locally

	if(is.na(x))
		x <- object$tpp.x		# store transportplan locally
	I <- length(object$warehouses)	
	J <- length(object$customers) 
	if(length(x) == I * J){

	    if(is.null(li$arrow.textposition )) li$arrow.textposition 	<- 2/3
	    if(is.null(li$arrow.Tips )) 		li$arrow.Tips 			<- .2
	    if(is.null(li$arrow.Code )) 		li$arrow.Code 			<- 2
	    if(is.null(li$arrow.lwd ))  		li$arrow.lwd 			<- 2 
	    if(is.null(li$arrow.bg.col ))  		li$arrow.bg.col 		<- 1
	    if(is.null(li$arrow.font.col )) 	li$arrow.font.col 		<- 1
	    if(is.null(li$arrow.font.cex )) 	li$arrow.font.cex 		<- .75
	    if(is.null(li$arrow.point.col )) 	li$arrow.point.col 		<- 1
	    if(is.null(li$arrow.point.pch )) 	li$arrow.point.pch 		<- 21
	    if(is.null(li$arrow.point.bg )) 	li$arrow.point.bg 		<- "white"
	    if(is.null(li$arrow.point.cex )) 	li$arrow.point.cex 		<- 4 
	    if(is.null(li$arrow.cex )) 			li$arrow.cex 			<- 1 

    	if(length(li$arrow.bg.col	)!=I) 	li$arrow.bg.col 		<- rep(li$arrow.bg.col, I)
    	if(length(li$arrow.font.col )!=I) 	li$arrow.font.col 		<- rep(li$arrow.font.col, I)
    	if(length(li$arrow.point.bg )!=I) 	li$arrow.point.bg 		<- rep(li$arrow.point.bg, I)

		for(i in 1:I){
			warehouse <- object$warehouses[[i]]
			for(j in 1:J){
				customer <- object$customers[[j]]
				if(x[i,j]> 0) { 
					arrows(
						warehouse$x,warehouse$y, 	# From 
						customer$x,customer$y, 		# TO
						code	= li$arrow.Code,
						length	= li$arrow.Tips,
						lwd		= li$arrow.lwd,
						col		= li$arrow.bg.col[i]
					) 
					if(   !li$isWLP){

						points( warehouse$x + li$arrow.textposition *(customer$x-warehouse$x),
								warehouse$y + li$arrow.textposition *(customer$y-warehouse$y), 
								pch=li$arrow.point.pch,
								cex=li$arrow.point.cex,
								bg =li$arrow.point.bg[i],
								col=li$arrow.point.col
						) 
						text(warehouse$x + li$arrow.textposition *(customer$x-warehouse$x),
							 warehouse$y + li$arrow.textposition *(customer$y-warehouse$y), 
							 x[i,j], 
							 cex=li$arrow.font.cex,
							 col=li$arrow.font.col
						) 

					}
				}
			} 
		}
	} 
 }
)