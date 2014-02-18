setGeneric("HNU.OR.TPP.SteppingStone.GetBestPolygonZug",  function(object,...)  standardGeneric("HNU.OR.TPP.SteppingStone.GetBestPolygonZug") )
 setMethod("HNU.OR.TPP.SteppingStone.GetBestPolygonZug", signature(object="HNUGeoSituation"),
  function(object,...){ 
 
  		li <- list(...)  
	  	if(is.null(li$log)) li$log <- TRUE 
	  	if(li$log){ 
    		# message("\tHNU.OR.TPP.SteppingStone.GetBestPolygonZug\n")
	  	}
 
		if(is.null(li$i)) 	 stop("Required Parameter i is missing.")
		if(is.null(li$j)) 	 stop("Required Parameter j is missing.") 

		i <- li$i
		j <- li$j

		# in case for one pair there exists more than one...
		# HNU.OR.TPP.SteppingStone.GetPolygonZuege should return only one
  		 
		foo<-NULL
		foo<-HNU.OR.TPP.SteppingStone.GetPolygonZuege(object,i_i = i,j_j =j, ...)
		Poly<-NULL
		if (!is.null(foo)){
			Y<-nrow(foo)
			Poly<-foo[1,]
			if (Y>1){
				for (l in (2:Y)){ 
					n.Poly<-foo[l,]
					if(n.Poly$opp < Poly$opp){
						Poly<-n.Poly
					}
				}
			}
		}
		return(Poly)
	}
)