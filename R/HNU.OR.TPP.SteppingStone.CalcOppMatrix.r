setGeneric("HNU.OR.TPP.SteppingStone.CalcOppMatrix",  function(object,...)  standardGeneric("HNU.OR.TPP.SteppingStone.CalcOppMatrix") )
 setMethod("HNU.OR.TPP.SteppingStone.CalcOppMatrix", signature(object="HNUGeoSituation"),
  function(object,...){  
		li <- list(...)  
	  	if(is.null(li$log)) li$log <- TRUE 
	  	if(li$log){ 
    		message("\tHNU.OR.TPP.SteppingStone.CalcOppMatrix")
	  	}
	  	M<- length(object$warehouses)
	 	N<- length(object$customers) 
	 	countBasisVariables <- M+N-1
		opp<-HNU.OR.getInitialMatrix(object, initialvalue=NA, ...)
		x<-object$tpp.x
		
		for (i in 1:M){
			for (j in 1:N){
				if ((x[i,j]==0)) { 
					p<-HNU.OR.TPP.SteppingStone.GetBestPolygonZug(object,i = i,j = j, ...)  
					if(!is.null(p)){ 
						opp[i,j]<-p$opp
					}else{
			 			message(paste("A Polygon-Cycle should have been calculated (x[i,j]:",x[i,j]," - i:",i,"/j:",j,")."))
						cat("calculated OPP.Cost matrix:\n")
						print(opp)
						cat("current Transportplan:\n")
						print(object$tpp.x)
						stop("A Polygon-Cycle should have been calculated (x[i,j]:",x[i,j]," - i:",i,"/j:",j,").")
					}
				}
			}
		} 
		if(sum(is.na(opp)) != countBasisVariables){
			message("ERROR in HNU.OR.TPP.SteppingStone.CalcOppMatrix.")
			
			res <- matrix(rep("OK", M*N), ncol = N)
			for(i in 1:M){
				for(j in 1:N){
					if( object$tpp.x[i,j] != 0 & !is.na(opp[i,j]) ){
						res[i,j] <- "should Not be OPP"
					}else if (object$tpp.x[i,j] == 0 &  is.na(opp[i,j])){
						res[i,j] <- "should be OPP"
					}
				}
			}
			cat("Errors in OPP.Cost matrix:\n")
			print(res)
			cat("calculated OPP.Cost matrix:\n")
			print(opp)
			cat("current Transportplan:\n")
			print(object$tpp.x)


			stop(paste("The number of opportunity costs is", sum(!is.na(opp)), 
				"and not as expected", (M*N -countBasisVariables),"!\n",
				"This indicates a bug in 'HNU.OR.TPP.SteppingStone.CalcOppMatrix'."))
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