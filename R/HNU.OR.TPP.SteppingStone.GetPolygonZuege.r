setGeneric("HNU.OR.TPP.SteppingStone.GetPolygonZuege",  function(object,...)  standardGeneric("HNU.OR.TPP.SteppingStone.GetPolygonZuege") )
 setMethod("HNU.OR.TPP.SteppingStone.GetPolygonZuege", signature(object="HNUGeoSituation"),
  function(object,...){ 
 
  		li <- list(...)  
		result<-NULL  #Rueckgabe 
	  	if(is.null(li$log)) li$log <- TRUE 
	  	if(li$log){ 
    		#message("\tHNU.OR.TPP.SteppingStone.GetPolygonZuege\n")
	  	} 
		
		if(is.null(li$i_i)) 	 stop("Required Parameter i_i is missing.")
		if(is.null(li$j_j)) 	 stop("Required Parameter j_j is missing.") 
		i_i <- li$i_i
		j_j <- li$j_j
		cij <- object$tpp.costs
		xij <- object$tpp.x
		 

		I<-nrow(cij)
		J<-ncol(cij)
		if(is.null(li$Poly)){
			#Ersten PolygonZug initialisieren
			n.Poly<-NULL
			n.Poly$List<-data.frame(i=i_i,j=j_j)
			n.Poly$opp<-cij[i_i,j_j]
			n.Poly$vertical<-TRUE 
			n.Poly$closed<-FALSE 
			n.Poly$NextIsAdd<-FALSE
			n.Poly$count<-1
			if(xij[i_i,j_j] >0){
				n.Poly$closed=TRUE
			}
			Poly <- n.Poly 
		}else{
			# UEbergebener Polygonzug
			Poly<-li$Poly
		}  


		if (Poly$closed){
			# n.Polies <- rbind(n.Polies,data.frame(PZ=Poly))  
			result <- rbind(result,Poly)  
		}else{ 
			if (Poly$vertical){
				#Suche in vertikaler Richtung
				j<-Poly$List$j[length(Poly$List$j)] 
				if (length(Poly$List$j)>1 & j==Poly$List$j[1]){
					#Polygonzug Finalisieren
					Poly$closed<-TRUE
					result<-rbind(result,Poly)
				}else{
					#Noch nicht belegte Zeilen Holen
					Y<-1:I 
					Y<-Y[-Poly$List$i] 
					Y<-cbind(Y,Poly$List$i[1]) 
					#... und ueber diese Iterieren 
					for (i in Y){
						# temp<-paste("x[",i,",",j,"] = ",xij[i,j])
						# print(temp) 
						n.Poly<-HNU.OR.TPP.SteppingStone.CalcPolyGonZug(object,i=i,j=j,Poly=Poly, ...)
						if (!is.null(n.Poly)){
							result<-rbind(result,n.Poly)
						}
					}
				}
			}else{
				#Suche in horizontaler Richtung
				i<-Poly$List$i[length(Poly$List$i)]  
				if (length(Poly$List$i)>1 & i==Poly$List$i[1]){
					#Polygonzug Finalisieren
					Poly$closed<-TRUE
					result<-rbind(result,Poly)
				}else{
					#Noch nicht belegte Spalten Holen
					X<-1:J
					X<-X[-Poly$List$j]
					X<-cbind(X,Poly$List$j[1]) 

					#... und ueber diese Iterieren
					for (j in X){ 
						# temp<-paste("x[",i,",",j,"] = ",xij[i,j])
						# print(temp)   
						n.Poly<-HNU.OR.TPP.SteppingStone.CalcPolyGonZug(object,i=i,j=j,Poly=Poly, ...)
						if (!is.null(n.Poly)){
							result<-rbind(result,n.Poly)
						}
					}
				}
			}
		} 
		if (!is.null(result)){
			r.result<-result 
			y<-nrow(r.result)

			result<-NULL 
			for (k in (1:y)){
				n.Poly<-r.result[k,]  
				if(n.Poly$closed){
					if(n.Poly$count>=4){
						result<-rbind(result,n.Poly)
					}
				}else{
					n.result<-HNU.OR.TPP.SteppingStone.GetPolygonZuege(object,i_i=i,j_j=j,Poly=n.Poly, ...)
					if (!is.null(n.result)){
						Y<-nrow(n.result)

						for (l in (1:Y)){ 
							l.Poly<-n.result[l,]
							result<-rbind(result,l.Poly)
						}
					}
				}
			} 
		}
		return(result)
	}
)
