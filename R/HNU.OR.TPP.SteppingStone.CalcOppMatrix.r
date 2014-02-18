setGeneric("HNU.OR.TPP.SteppingStone.CalcOppMatrix",  function(object,...)  standardGeneric("HNU.OR.TPP.SteppingStone.CalcOppMatrix") )
 setMethod("HNU.OR.TPP.SteppingStone.CalcOppMatrix", signature(object="HNUGeoSituation"),
  function(object,...){  
		li <- list(...)  
	  	if(is.null(li$log)) li$log <- TRUE 
	  	if(li$log){ 
    		message("\tHNU.OR.TPP.SteppingStone.CalcOppMatrix\n")
	  	}

		opp<-HNU.OR.getInitialMatrix(object, initialvalue=NA, ...)
		 
		for (i in 1:nrow(opp)){
			for (j in 1:ncol(opp)){
				if ((x[i,j]==0)) { 
					p<-HNU.OR.TPP.SteppingStone.GetBestPolygonZug(object,i = i,j = j, ...)  
					if(!is.null(p)){ 
						opp[i,j]<-p$opp
					}
				}
			}
		} 
		object$tpp.costs.opp <-opp
		if(li$log){ 
			message(paste("\t\tOpp-Cost-Matrix: \n"))
			print(opp) 
			cat("\n")
		}
		return(object)
	} 
)