setGeneric("HNU.OR.TPP.SteppingStone.CalcPolyGonZug",  function(object,...)  standardGeneric("HNU.OR.TPP.SteppingStone.CalcPolyGonZug") )
 setMethod("HNU.OR.TPP.SteppingStone.CalcPolyGonZug", signature(object="HNUGeoSituation"),
  function(object,...){ 
  
		li <- list(...) 
 		
		if(is.null(li$i)) 	 stop("Required Parameter i is missing.")
		if(is.null(li$j)) 	 stop("Required Parameter j is missing.")
		if(is.null(li$Poly)) stop("Required Parameter Poly is missing.")

	  	if(is.null(li$iter.max)) li$iter.max <- -1 
	  	if(is.null(li$log)) li$log <- TRUE 

		i<- li$i
	  	j<- li$j
	  	Poly <- li$Poly 
		n.Poly<-NULL
		x <- object$tpp.x
		cij <- object$tpp.costs

		if(li$log){ 
    		# message("\tHNU.OR.TPP.SteppingStone.CalcPolyGonZug(i=",i,", j=",j," , ...)\n")
	  	}
	  	
		if(x[i,j]>0){
			#Wenn das Aktuelle Element eine Basisvariable ist, 
			#Erstelle einen neuen Polygonzug
			n.Poly<-Poly
			#Fuege die aktuelle Position als Element hinzu
			n.Poly$List<-rbind(n.Poly$List,data.frame(i=i,j=j))
			#und addiere/subtrahiere die Kosten 
			#zur Berechnung der Opportunitaet
			# print(n.Poly$opp)
			if(n.Poly$NextIsAdd){
				n.Poly$opp <- n.Poly$opp + cij[i,j]
			}else{
				n.Poly$opp <- n.Poly$opp - cij[i,j]
			} 
			#Beim naechsten mal genau andersrum :)
			n.Poly$NextIsAdd <-!n.Poly$NextIsAdd
			n.Poly$vertical	 <-!n.Poly$vertical
			n.Poly$count	 <- n.Poly$count+1
			 
		}
		return(n.Poly)
	}
) 