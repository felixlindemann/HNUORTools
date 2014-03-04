setGeneric("HNU.OR.WLP.ADD",  function(object,...)  standardGeneric("HNU.OR.WLP.ADD") )
 setMethod("HNU.OR.WLP.ADD", signature(object="HNUGeoSituation"),
  function(object,...){ 
    message("This is HNU.OR.WLP.ADD")
    li<-list(...)   
	
	I <- length(object$warehouses)
	J <- length(object$customers)

  	if(is.null(li$cij)){
  		li$cij <- object$wlp.costs
  	} 

	if(length(li$cij) ==1){
		stop("no costs matrix given. Problem not solveable.")
	}
  	if(nrow(li$cij)!=I){
  		stop("The number of rows in the given costs-matrix is wrong. (Expected:",I," given:",nrow(li$cij),")")
  	}
  	if(ncol(li$cij)!=J){
  		stop("The number of columns in the given costs-matrix is wrong. (Expected:",J," given:",ncol(li$cij),")")
  	}

  	if(is.null(li$maxiter)) li$maxiter <- I * 2 +1 # make sure, it will not abort early
  	if(is.null(li$log)) li$log <- FALSE
  	if(is.null(li$plot)) li$plot <- FALSE

  	if(li$log){

  		cat("############ Initial Iteration: ############\n\nUsing cij:\n")
  		print(li$cij)
  	}
	object$wlp.costs <- li$cij
	 
	# prepare result as list 
	result <- list()
	result$Iteration<-0 

	result$x <-object$wlp.costs*0
	result$y <-rep(0,I)

	#verwerfe alle Laeger vorlaeufig
	result$IOVL<-1:I 	#init Vorlaeuig  verworfene
	result$IO<-integer()   	#init endgueltig verworfene
	result$I1<-integer()		#init endgueltig aufgenommene


	#bereite w-Matrix vor
	result$w<-data.frame(matrix(rep(0,I*(J+3)),ncol=J+3))
	rownames(result$w)<-  sapply(object$warehouses, function(o){o$id})
	colnames(result$w)<-c(sapply(object$customers , function(o){o$id}),"wi","fi","wifi")
	result$w$fi<-sapply(object$warehouses , function(o){o$fixcosts})
	  

	#bereite cij-Matrix vor
	result$cij<-data.frame(matrix(rep(0,I*(J+3)),ncol=J+3))
	rownames(result$cij)<-  sapply(object$warehouses, function(o){o$id})
	colnames(result$cij)<-c(sapply(object$customers , function(o){o$id}),"ci","fi","cifi")
	result$cij$fi<-sapply(object$warehouses , function(o){o$fixcosts})
	result$cij[,1:J] 	<- 	object$wlp.costs
	result$cij$ci 	 	<-	rowSums(object$wlp.costs)
	result$cij$cifi 	<-	result$cij$ci + result$cij$fi
	
	# suche das guenstigste Lager fuer alleinige Belieferunge
	cij.min <- max(result$cij$cifi)*2+1
	k <- NA
	for(i in 1:I){
		object$warehouses[[i]]$open <- FALSE
		if(result$cij$cifi[i] < cij.min){
			cij.min <- result$cij$cifi[i]
			k <- i
		}
	} 
	if(is.na(i)){
		stop("This shouldn't happen. could not find cheapest warehouse.")
	}


	if(li$plot){ 
		dev.new()
		plotGeoSituation(object, ...) # fuer Aufgabenstellung
	}
	# Setzte I1
	result$I1 <- c(result$I1, k)
	# update IOVL
	result$IOVL <- result$IOVL[-which(result$IOVL== k )]
	#setze gesamtkosten
	result$totalcosts <- cij.min
 		 
	#setze Ergebnisse
	result$y[k] <- TRUE
	result$x[k,] <- 1
	object$warehouses[[k]]$open <- TRUE

	object$wlp.solution <- result 
	if(li$log){

		cat("Berechnete C(ij)-Matrix:\n")
		print(result$cij)
		cat("Ausgewähltes Lager: ", k, "(k.min=",cij.min,")\n")
		cat("Definierte Mengen\n")
		cat("\tI1:   {", result$I1, "}\n")
		cat("\tIO:   {", result$IO, "}\n")
		cat("\tIOVL: {", result$IOVL, "}\n")
    
		cat("Current Soultion x(ij)-Matrix:\n")
		print(result$x)

		cat("Current Soultion y(i):\n")
		print(result$y)

	}	
	if(li$plot){ 
		dev.new() 
		plotGeoSituation(object, ...)
		plotGeoSituation.transportplan(object, ...)
		title(sub=paste("Totalcosts: ",round(result$totalcosts) ))
	}
	# iteriere solange, wie Laeger vorlaeufig verworfen sind
	while(length(result$IOVL)>0 && result$Iteration < li$maxiter){
		result$Iteration<-result$Iteration+1
		if(li$log){
			cat("############# Beginne Iteration ",result$Iteration," #############\n\n")
		}

		#update WIJ
		
		#Berechne die Matrix W fuer alle i in IOVL und alle j
		#wie folgt: wij = max(ckj-cij,0)
		for(i in result$IOVL){ 
			#iteriere ueber alle Spalten
			for(j in 1:J){
				#setze einsparung des Kunden gemaess max(0,...)
				if(result$Iteration == 1){
					result$w[i,j]<-max(cij[k,j]-cij[i,j],0) 
				}else{
					result$w[i,j]<-max(result$w[i,j]-result$w[k,j],0) 
				} 
			} 
		} 

		#update wi/Wifi
		result$w$wi 	<- rowSums(result$w[ ,1:J])
		result$w$wifi 	<- result$w$wi - result$w$fi 

	 	if(li$log){
  
			cat("Berechnete W(ij)-Matrix:\n")
			print(result$w[result$IOVL, ])
		}

		# verwerfe alle laeger endgueltig, deren Gesamtersparnisse negativ sind
	 	IO.tmp <- result$IOVL[result$w$wifi[result$IOVL] <= 0]
	 	if(li$log){
	 		cat("Verwerfe folgende Standorte:", IO.tmp, "\n")
	 	}
	 	for(l in IO.tmp){
			result$IO <- c(result$IO, l)
			result$IOVL <- result$IOVL[-which(result$IOVL == l )] 
	 	} 
	 	if(li$log){
			cat("Definierte Mengen\n")
			cat("\tIOVL: {", result$IOVL, "}\n")
			cat("\tI1:   {", result$I1, "}\n")
			cat("\tIO:   {", result$IO, "}\n") 
		}	

		if(length(result$IOVL) == 0){
			if(li$log){
				cat("Add-Algorithm terminates because IOVL is empty.\n")
			}
			break # no further Storage found
		}

		#finde next lager
		k<- NA
		w.max <- 0 
		for(i in result$IOVL){
			if(result$w$wifi[i] > w.max){
				w.max <- result$w$wifi[i]
				k <- i
			}
		}
		if(is.na(k)) stop("This shouldn't happen. could not find cheapest warehouse.")
		#update I1 
 		result$I1 <- c(result$I1, k)
 		# Update IOVL
		result$IOVL <- result$IOVL[-which(result$IOVL== k )]
		#Update F
		result$totalcosts <-result$totalcosts - w.max

		#Update Entscheidungsvariable
		result$y[ k] <- TRUE
		result$x[ k,which(result$w[k,1:J] >0)] <- 1
		result$x[-k,which(result$w[k,1:J] >0)] <- 0
		object$warehouses[[k]]$open <- TRUE
		object$wlp.solution <- result 

		if(li$log){
 
			cat("Ausgewähltes Lager: ", k, "(Ersparnis:",w.max,")\n")
			cat("Aktuelle Lösung: ", result$totalcosts , "\n")
			cat("Definierte Mengen\n")
			cat("\tIOVL: {", result$IOVL, "}\n")
			cat("\tI1:   {", result$I1, "}\n")
			cat("\tIO:   {", result$IO, "}\n")

			cat("Current Soultion x(ij)-Matrix:\n")
			print(result$x)

			cat("Current Soultion y(i):\n")
			print(result$y)

		}	
	 
		if(li$plot){ 
			dev.new() 
			plotGeoSituation(object, ...)
			plotGeoSituation.transportplan(object, ...)
			title(sub=paste("Totalcosts: ",round(result$totalcosts) ))
		}
		if(length(result$IOVL) ==0) 
			break
		if(result$Iteration >= li$maxiter)
			break
	} 
	if(li$log){
			message("Target-Value:",result$totalcosts,"\n")
		    message("This was(!) HNU.OR.WLP.ADD")
	}
	return(object)
  }
)