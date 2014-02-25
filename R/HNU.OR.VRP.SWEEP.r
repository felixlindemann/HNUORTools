setGeneric("HNU.OR.VRP.SWEEP",  function(object,...)  standardGeneric("HNU.OR.VRP.SWEEP") )
 setMethod("HNU.OR.VRP.SWEEP", signature(object="HNUGeoSituation"),
  function(object,...){ 
    message("HNU.OR.VRP.SWEEP\n")
  	li <- list(...) 

  	M <- length(object$warehouses)
  	N <- length(object$warehouses)
  	if(length(object$tpp.x) == 1){

  		if(M!=1)
  			stop("There is no Transportplan assigned. This is required as there are more than 1 warehouses.")

  		x <- matrix(sapply(object$customers, function(o){o$demand}), nrow=1, byrow=TRUE)

  		rownames(x) <- sapply(object$warehouses, function(o){o$id})
  		colnames(x) <- sapply(object$customers,  function(o){o$id})

  		object$tpp.x <- x

  	}else{

	  	if(nrow(object$tpp.x)!=M)
	  		stop("There is no valid Transportplan assigned. The number of warehouses is not equal to the number of rows in the transportation plan.")

	  	if(ncol(object$tpp.x)!=N)
	  		stop("There is no valid Transportplan assigned. The number of customers is not equal to the number of columns in the transportation plan.")

  	}

	if(!is.null(li$cij)){
		object$tsp.costs <- li$cij
	}
  	if(length(object$tsp.costs) == 1){
 	
 		nodes <- list()

 		for ( i in 1:M){

 			nodes[length(nodes)+1] <- object$warehouses[[i]]

 		}

 		for ( j in 1:M){

 			nodes[length(nodes)+1] <- object$customers[[j]]

 		}

 		K <- N+M

 		cij <- matrix(rep(0, K*K), ncol=K, nrow=K, byrow=TRUE)

 		for (i in 1:(K-1))
 		{
 			cij[i,i] <-0
 			n1 <- nodes[[i]]

 			for(j in (i+1):K){
 				n2 <- nodes[[j]]
 				cij[i,j] <- calc.Distance(n1,n2,...)
 				cij[j,i] <- cij[i,j]
 			}
 			
 		}
 		object$tsp.costs <- cij
  	}else{	

	  	if(nrow(object$tsp.costs)!=M)
	  		stop("There is no valid costmatrix assigned. The number of warehouses is not equal to the number of rows in the transportation plan.")

	  	if(ncol(object$tsp.costs)!=N)
	  		stop("There is no valid costmatrix assigned. The number of customers is not equal to the number of columns in the transportation plan.")

  	}
 

  	if(is.null(li$vehiclecapacity.time))   	
	{	
		li$vehiclecapacity.time <- sum(object$tsp.costs)*2+1
		warning("Maximum Vehicle Time Capacity is not given. providing default value.")
  	}
  	if(is.null(li$vehiclecapacity.capacity)) 
	{	
		li$vehiclecapacity.capacity <- sum(object$tpp.x)*2+1
		warning("Maximum Vehicle Loading Capacity is not given. providing default value.")
	}
  	if(is.null(li$vehiclecapacity.length)) 
	{
		li$vehiclecapacity.length <- sum(object$tsp.costs)*2+1
		warning("Maximum Tour-Length is not given.providing default value.")
	}


  	for(i in 1:M){

  		w<- object$warehouses[[j]]
  		df<-data.frame()
  		for(j in 1:N){ 
  			if(object$tpp.x[i,j] >0){
  				cust <- object$customers[[j]] 
  				df<-rbind(df, data.frame(j = j, polar= calc.polar(w,cust, ...) ))
  			} 
  		}
  		# sort by col2.
  	}

  })
VRP.SWEEP <-
function(
	cij,
	Warehouse,Customers,
	DrehrichtungIUZ=TRUE,
	alpha=0,
	alphaIsDeg=FALSE,
	CapL=NA,
	CapT=NA,
	CapN=NA,
	main="Vehicle Routing Problem",sub="",
	PLOT=FALSE,
	PLOTASFILE=FALSE,
	FILENAME="VRP_Savings",
	DEBUG=FALSE,
	ColorizeRoutes=TRUE,
	ConnectDepot=FALSE,
	MaxIterationen=NA,
	Bounds=NULL,
	drawGrid=FALSE,
	Grid=c(10,50),
	zoom=2.5,
	bg.col.Origin=NULL,
	bg.col.Destination=NULL,
	border.col="black",
	font.cex=1,
	font.col="black",
	point.pch=20,
	point.cex=1
){
	VRP <- new.env() #neuer Loesungsraum  
	
	iteration<-0	 
	AnzahlTouren<-0
	F<-0
	s<-NULL
	Customers<-cbind(Customers,onTour=NA,rang=0,alpha=0)
	if(alphaIsDeg == TRUE){
		alpha<-alpha/180*pi
	}
	#nur positive Alpha als Startwinkel
	if(alpha<0){
		alpha<-alpha+2*pi
	}
	#berechne Winkel
	for(i in (1:nrow(Customers))){ 
		# Berechne Winkel
		p<-getPhi(Customers[i,"x"],Customers[i,"y"],
			 x0=Warehouse[1,"x"],y0=Warehouse[1,"y"],DEG=FALSE)
		p<-p-alpha
		if(p<0){
			p<-p+2*pi
		}
		Customers[i,"alpha"]<-p
		if(DEBUG==TRUE){
			cat("Berechne Polarwinkel ",Customers[i,"name"],": ",Customers[i,"alpha"],"\n")
		}
	} 
	if(is.na(MaxIterationen)){
		MaxIterationen<-nrow(Customers)
	}
 
	assign(paste("Warehouse",sep="."), Warehouse[1,], envir=VRP)  
	#Rangfolge berechnen 
	assign(paste("Customers",sep="."), Customers, envir=VRP)  
	Customers <- Customers[order(Customers$alpha, decreasing = DrehrichtungIUZ),]
	Customers$rang <- 1:(nrow(Customers))
	
	if(DEBUG==TRUE){
		cat("\n\nTourenbilden:\n")
	}
	Tour<-NULL
	AnzahlTouren<-0
	for(i in 1:nrow(Customers)){
		if(DEBUG==TRUE){
			cat("Untersuche Kunde: ",Customers[i,"name"],"\tb: ",as.numeric(Customers[i,"b"]),"\n")
		}
		if(MaxIterationen < iteration){
			if(DEBUG==TRUE){
				cat("\n\nAbbruch nach Iteration ",iteration," --> MaxIterationen=",MaxIterationen,"\n\n")
			}
			break
		}
		iteration<-iteration+1
		if(DEBUG==TRUE){
			cat("Aktuelle Tour == null --> ",is.null(Tour),"\n")
		}
		
		if(is.null(Tour)){
			# Neue Tour bilden
			if(DEBUG==TRUE){cat("\n\n------------- neue Tour ----------------\n\n")}
			AnzahlTouren<-AnzahlTouren+1 
			Tour$Index  	<-	AnzahlTouren
			Tour$Customers 	<-	c(Customers[i,"name"])
			Tour$CapL 		<-	as.numeric(Customers[i,"b"])
			Tour$CapN 		<-	length(Tour$Customers)
			Tour$CapT 		<-  as.numeric(cij[Warehouse[1,"name"],Customers[i,"name"]])

			Tour$Created 	<- iteration
			Tour$Abandoned	<- NA

			if(DEBUG==TRUE){
				cat("Erstelle Neue Tour (",Tour$Index,")")
			}
			
			if(!is.na(CapL)){
				Tour$FreeCapL<- as.numeric(CapL - Tour$CapL )
				if(DEBUG==TRUE){
					cat("\tTour$FreeCapL: ",Tour$FreeCapL)
				}
			}
			if(!is.na(CapT)){
				Tour$FreeCapT<- as.numeric(CapT - Tour$CapT)
				if(DEBUG==TRUE){
					cat("\tTour$FreeCapT: ",Tour$FreeCapT)
				}
			}
			if(!is.na(CapN)){
				Tour$FreeCapN<- as.numeric(CapN - Tour$CapN)
				if(DEBUG==TRUE){
					cat("\tTour$FreeCapN: ",Tour$FreeCapN)
				}
			}
			if(DEBUG==TRUE){
				cat("\n")
			}
			Customers$onTour[i]<-Tour$Index
			    
		 	
		}else{
		 	# Tour kann ergaenzt werden
			if(DEBUG==TRUE){
				cat("\tTour$CapL: ",Tour$CapL,"\tTour$CapT: ",Tour$CapT,"\tTour$CapN: ",Tour$CapN,"\n")
			}
			capl<-Tour$CapL + Customers[i,"b"]
			capt<-Tour$CapT + cij[Tour$Customers[Tour$CapN],Customers[i,"name"]] + cij[Customers[i,"name"],Warehouse[1,"name"]]
			capn<-Tour$CapN + 1

			canBeJoined<-TRUE
			cat("canBeJoined == TRUE && !is.na(CapL): ",canBeJoined,"\n")
			if(canBeJoined == TRUE && !is.na(CapL)){
				canBeJoined = (CapL > capl)
				if(DEBUG==TRUE && canBeJoined==FALSE){
					cat("\n cannot BeJoined --> Loading Capacity (CapL > capl --> ",CapL,">",capl," --> ",canBeJoined,")\n")
				}
			}
			cat("canBeJoined == TRUE && !is.na(CapT): ",canBeJoined,"\n")
			if(canBeJoined == TRUE && !is.na(CapT)){
				canBeJoined = (CapT > capt)
				if(DEBUG==TRUE && canBeJoined==FALSE){
					cat("\n cannot BeJoined --> Time Capacity (CapT > capl --> ",CapT,">",capt," --> ",canBeJoined,")\n")
				}
			}
			cat("canBeJoined == TRUE && !is.na(CapN): ",canBeJoined,"\n")
			if(canBeJoined == TRUE && !is.na(CapN)){
				canBeJoined = (CapN > capn)
				if(DEBUG==TRUE && canBeJoined==FALSE){
					cat("\n cannot BeJoined --> Stop Capacity (CapN > capl --> ",CapN,">",capt," --> ",canBeJoined,")\n")
				}
			}
			cat("canBeJoined == TRUE: ",canBeJoined,"\n")
			print(canBeJoined)
			if(canBeJoined == TRUE){
				# Ok
				if(DEBUG==TRUE){
					cat("Fuege Kunde ",Customers[i,"name"]," zur Tour ",Tour$Index," hinzu.\n")
				}
				Tour$CapL<-Tour$CapL + Customers[i,"b"]
				Tour$CapT<-Tour$CapT + cij[Tour$Customers[Tour$CapN],Customers[i,"name"]]
				Tour$CapN<-Tour$CapN + 1
				Tour$Customers<-c(Tour$Customers,Customers[i,"name"])
				if(!is.na(CapL)){
					Tour$FreeCapL<- as.numeric(Tour$FreeCapL -  Customers[i,"b"]) 
				}
				if(!is.na(CapT)){
					Tour$FreeCapT<- as.numeric(Tour$FreeCapT -  cij[Tour$Customers[Tour$CapN],Customers[i,"name"]]) 
				}
				if(!is.na(CapN)){
					Tour$FreeCapN<- as.numeric(Tour$FreeCapN - 1) 
				}
				if(DEBUG==TRUE){
					cat("gehe zu naechstem Kunde \n")
				}
			}else{
				#Neue Tour 
				if(DEBUG==TRUE){
					cat("Beende Tour ",Tour$Index," .\n")
				}
				for(t in Tour$Customers){
					Customers[t,"onTour"]<-Tour$Index					
				} 
				Tour$CapT<-Tour$CapT + cij[Tour$Customers[Tour$CapN],Warehouse[1,"name"]] 
				assign(paste("Tour",Tour$Index,sep="."),Tour, envir=VRP)
				 

				if(DEBUG==TRUE){cat("\n\n------------- neue Tour ----------------\n\n") 
					cat("Erstelle Tour ",AnzahlTouren+1," .\n")
				}
				# Neue Tour bilden
				AnzahlTouren<-AnzahlTouren+1
				Tour<-NULL
				Tour$Index  	<-	AnzahlTouren
				Tour$Customers 	<-	c(Customers[i,"name"])
				Tour$CapL 		<-	as.numeric(Customers[i,"b"])
				Tour$CapN 		<-	length(Tour$Customers)
				Tour$CapT 		<-  as.numeric(cij[Warehouse[1,"name"],Customers[i,"name"]])

				Tour$Created 	<- iteration
				Tour$Abandoned	<- NA

				
				if(!is.na(CapL)){
					Tour$FreeCapL<- as.numeric(CapL - Tour$CapL )
				}
				if(!is.na(CapT)){
					Tour$FreeCapT<- as.numeric(CapT - Tour$CapT)
				}
				if(!is.na(CapN)){
					Tour$FreeCapN<- as.numeric(CapN - Tour$CapN)
				}
				Customers$onTour[i]<-Tour$Index
				    
			}
   		}		 
	} 
	if(DEBUG==TRUE){
		cat("Alle Kunden auf Touren verteilt. - Beende Tour\n")
	}
	for(t in Tour$Customers){
		Customers[t,"onTour"]<-Tour$Index					
	} 
	 
	Tour$CapT<-Tour$CapT + cij[Tour$Customers[Tour$CapN],Warehouse[1,"name"]] 
	assign(paste("Tour",Tour$Index,sep="."),Tour, envir=VRP)
	x<-cij*0
	n<-0


	if(DEBUG==TRUE){
		cat("Fuelle xij\n")
	}
	for(i in 1:AnzahlTouren){
		Tour<-get(paste("Tour",i,sep="."), envir=VRP)
		if(is.na(Tour$Abandoned)){
			n<-n+1 
			x.t<-cij*0
			x.t[Warehouse$name[1],Tour$Customers[1]]<-1
			if(length(Tour$Customers) >1){
				for(k in 2:length(Tour$Customers)){ 
					x.t[Tour$Customers[k-1],Tour$Customers[k]]<-1
				}
			}
			x.t[Tour$Customers[length(Tour$Customers)],Warehouse$name[1]]<-1
			Tour$CapT<- sum(cij*x.t)
			assign(paste("Tour",i,sep="."),Tour, envir=VRP)
			x<-x+x.t 
		}
	}
	if(DEBUG==TRUE){
		cat("Berechne Kosten\n")
	}
	F.final<-sum(cij*x)
	
	if(PLOT==TRUE){
		if(is.null(Bounds)){
			Bounds<-c(min(Warehouse$x,Warehouse$y,Customers$x,Customers$y),
					  max(Warehouse$x,Warehouse$y,Customers$x,Customers$y))
		}
		draw.geosituation (Bounds,Origins=Warehouse,Destinations=Customers,
			main=main,zoom=zoom,drawGrid=drawGrid,
			Grid=Grid,	
			bg.col.Origin=bg.col.Origin,
			bg.col.Destination=bg.col.Destination,
			border.col=border.col,
			font.cex=font.cex,
			font.col=font.col,
			point.pch=point.pch,
			point.cex=point.cex)
		for(i in 1:AnzahlTouren){ 
			tn<-paste("Tour",i,sep=".") 
			T<-get(paste("Tour",i,sep="."), envir=VRP)		

			col<-1
			if(ColorizeRoutes==TRUE){
				col<-i+1
			}	
			n<-T$CapN
			if(n>1){				
				for(k in 2:n){
					arrows(Customers[T$Customers[k-1],"x"],
						   Customers[T$Customers[k-1],"y"],
						   Customers[T$Customers[k  ],"x"],
						   Customers[T$Customers[k  ],"y"],col=col,length=0.2,angle=15)	
				}				
			}
			if(ConnectDepot==TRUE){
				arrows(Warehouse[1,"x"],Warehouse[1,"y"],Customers[T$Customers[1],"x"],Customers[T$Customers[1],"y"],col=col,length=0.2,angle=15)
				arrows(Customers[T$Customers[n],"x"],Customers[T$Customers[n],"y"],Warehouse[1,"x"],Warehouse[1,"y"],col=col,length=0.2,angle=15)
			}	
		}
		
	}
	
	#Bereite die Rueckgabe vor   
	assign("AnzahlTouren", AnzahlTouren, envir=VRP)
	assign("x", x, envir=VRP)
	assign("F.final", F.final, envir=VRP)
	assign("Customers", Customers, envir=VRP)
	as.list(VRP) 
}
