setGeneric("HNU.OR.VRP.SAVINGS",  function(object,...)  standardGeneric("HNU.OR.VRP.SAVINGS") )
 setMethod("HNU.OR.VRP.SAVINGS", signature(object="HNUGeoSituation"),
  function(object,...){ 
    message("HNU.OR.VRP.SAVINGS\n")
  	li <- list(...) 

  	M <- length(object$warehouses)
  	N <- length(object$customers)
  	if(is.null(li$log)) li$log <- TRUE
    if(is.null(li$alpha)) li$alpha <- 1
  	if(is.null(li$round.cij)) li$round.cij <- TRUE
  	totalcosts <- 0
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
	
  	if(length(object$tsp.costs) <= 1){
 	
   		nodes <- list()

   		for ( i in 1:M){

   			nodes[[length(nodes)+1]] <- object$warehouses[[i]]

   		}

   		for ( j in 1:N){

   			nodes[[length(nodes)+1]] <- object$customers[[j]]

   		}

   		K <- length(nodes)

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
   		if(li$round.cij)
   			cij <- round(cij)
   		object$tsp.nodes <- nodes
   		object$tsp.costs <- cij
   		if(li$log){
   			warning("\nCij has been calculated.\nNodes have been assigned to TSP.\n") 
        print(cij)
   		}
 		
  	}else{	

	  	if(nrow(object$tsp.costs)!=M)
	  		stop("There is no valid costmatrix assigned. The number of warehouses is not equal to the number of rows in the transportation plan.")

	  	if(ncol(object$tsp.costs)!=N)
	  		stop("There is no valid costmatrix assigned. The number of customers is not equal to the number of columns in the transportation plan.")

  	}
 	  cij <- object$tsp.costs
 
  	if(is.null(li$vehiclecapacity.maxstops)) {	
  		li$vehiclecapacity.maxstops <- N*2+1
  		warning("Maximum number of stops is not given. Constraint not taken into account.")
  	}
  	if(is.null(li$vehiclecapacity)) {	
  		li$vehiclecapacity <- sum(object$tpp.x)*2+1
  		warning("Maximum Vehicle Loading Capacity is not given. Constraint not taken into account.")
  	}  


  	for(i in 1:M){
  		# iterate over all warehouses
  		w<- object$warehouses[[i]]
  		if(li$log){
  			cat("Analyzing warehouse:",w$label,"\n")
  		}

      vrp <- list()
      vrp$tours <- list()
      
      vrp$customers<-df
      vrp$maxstops <- li$vehiclecapacity.maxstops 
      vrp$maxcapacity <- li$vehiclecapacity
      vrp$x   <- object$tsp.costs * 0
      vrp$cij <- object$tsp.costs
      vrp$totalcosts <-0
      # Erzeuge Pendeltouren
      df<-data.frame()  
  		for(j in 1:N){ 
  			if(object$tpp.x[i,j] >0){
  				cust <- object$customers[[j]] 

          tour <- list()
          tour$loading <- 0
          tour$costs <- 0 
          tour$stops <-0
          tour$stops.list    <- list()
          tour$stops.indices <- list()
          tour$loading <- cust$demand
          tour$stops <- 1
          tour$stops.list[[1]] <- cust
          tour$stops.indices[[1]] <- j # store customer index 
        
          tour$costs <-  object$tsp.costs[i,j+M] + object$tsp.costs[j+M,i]
          if(li$log){
            cat("\tPendel-Tour:",length(vrp$tours)+1,",Costs,", tour$costs ,"\n")
          }
          vrp$tours[[length(vrp$tours)+1]] <- tour   
          vrp$x[i,j]<- 1
          vrp$x[j,i]<- 1
  				df<-rbind(df, 
              data.frame(j = j, 
                tour=length(vrp$tours), 
                position=1, isEndCustomer = TRUE))
          vrp$totalcosts <- vrp$totalcosts+tour$costs

  			} 
  		} 
      # Calculate Savings
      sav<-data.frame()  
      if(li$log)cat("\tCalculating Savings-Matrix:\n")
        
      for(k in 1:(nrow(df)-1)){
        for(j in (k+1):nrow(df)){

          s <- object$tsp.costs[i,df$j[k]+M] + 
               object$tsp.costs[df$j[j]+M,i] - li$alpha * 
               object$tsp.costs[df$j[k]+M,df$j[j]+M]
          snetto <- object$tsp.costs[i,df$j[k]+M] + 
               object$tsp.costs[df$j[j]+M,i] -  
               object$tsp.costs[df$j[k]+M,df$j[j]+M]
          if(li$log){
            cat("\ti: ", df$j[k], "/j:", df$j[j], " --> ")
            cat(object$tsp.costs[i,df$j[k]+M])
            cat("+")
            cat(object$tsp.costs[df$j[j]+M,i])
            cat("-", li$alpha , "*" )
            cat(object$tsp.costs[df$j[k]+M,df$j[j]+M])
            cat("=", s, "\n")
          }
          sav<-rbind(sav, data.frame(sav= s, savnetto = snetto,
              i=df$j[k], j=df$j[j],  
              dfi = k+M, dfj = j+M,
              From=object$customers[[df$j[k]]]$label,
              To=object$customers[[df$j[j]]]$label,
              checked=FALSE, used=FALSE))
        }
      }

      sav <- sav[ order( -sav[,"sav"]), ] 
      if(li$log){
        cat("Savings-Tabelle:\n")
        print(sav)
      }
      if(li$log){
        cat("################## Vorbereitung abgeschlossen ##################\nBeginne Iterationen:\n") 
      }
      for(s in 1:nrow(sav)){

        if(li$log){ 
          cat("Checking Savings #",s,":",
              sav[s,"From"],"-",
              sav[s,"To"],". Savings:",
              sav[s,"sav"],"\n")
        } 
        if(sav[s,"sav"]<=0) { 
          message("Terminating Savings Algorithm: No more positive Savings found.")
          break 
        }
        skip<-FALSE
        df.i <- df[sav$i[s],]
        df.j <- df[sav$j[s],]
        if(li$log) {
          print(df.i)
          print(df.j)
        }
        if(df.i$tour == df.j$tour){
          skip<-TRUE
          if(li$log) cat("\t Skipping Savings: Both Customers are on same tour.\n")
        }
        if(df.i$isEndCustomer == FALSE){
          skip <- TRUE
          if(li$log) cat("\t Skipping Savings: ",sav[s,"From"]," is not an end-customer.\n") 
        }
        if(df.j$isEndCustomer == FALSE){
          skip <- TRUE
          if(li$log) cat("\t Skipping Savings: ",sav[s,"To"]," is not an end-customer.\n") 
        }
        t.1 <- vrp$tours[[df.i$tour]]
        t.2 <- vrp$tours[[df.j$tour]]
        if(t.1$loading + t.2$loading > li$vehiclecapacity){
          skip <- TRUE
          if(li$log) cat("\t Skipping Savings: laoding capacity will be exceeded: ",t.1$loading ,"+", t.2$loading ,">", li$vehiclecapacity,".\n") 

        }
        if(length(t.1$stops) + length(t.2$stops)  > li$vehiclecapacity.maxstops){
          skip <- TRUE
          if(li$log) cat("\t Skipping Savings: maximum number of Stops will be exceeded: ",length(t.1$stops) ,"+", length(t.2$stops)  ,">", li$vehiclecapacity.maxstops,".\n") 

        }
        if(!skip){
          # verbinde Touren
          if(li$log) cat("Creating new Tour: t",(length(vrp$tours)+1),"--> Combining Tours t1:",df.i$tour ,"& t2:", df.j$tour,".\n") 
          tour <- list()
          tour$loading <- t.1$loading + t.2$loading 

          tour$costs <- t.1$costs + t.2$costs - sav$savnetto[s]
          vrp$totalcosts <-  vrp$totalcosts - sav$savnetto[s]
          if(li$log){
            cat("\tnew Capacity:", tour$loading ," - Costs:", tour$costs ,"\n")
          }
          
          
          tour$stops <-t.1$stops + t.2$stops
          tour$stops.list    <- list()
          tour$stops.indices <- list()  
            
          c1.isEndCustomer <- df.i$position > 1
          c2.isEndCustomer <- df.j$position > 1 
          if(c1.isEndCustomer){
            if(c2.isEndCustomer){
              tour$stops.list <- c( (t.1$stops.list), rev(t.2$stops.list))
              tour$stops.indices <- c( (t.1$stops.indices), rev(t.2$stops.indices))
            }else{
              tour$stops.list <- c( (t.1$stops.list),  (t.2$stops.list))
              tour$stops.indices <- c( (t.1$stops.indices),  (t.2$stops.indices))
            }
          }else{
            if(c2.isEndCustomer){
              tour$stops.list <- c(rev(t.1$stops.list),  rev(t.2$stops.list))
              tour$stops.indices <- c(rev(t.1$stops.indices),  rev(t.2$stops.indices))
            }else{
              tour$stops.list <- c(rev(t.1$stops.list), (t.2$stops.list))
              tour$stops.indices <- c(rev(t.1$stops.indices), (t.2$stops.indices))
            }
          }
 
          vrp$tours[[df.i$tour]] <- list()  #resect current tour
          vrp$tours[[df.j$tour]] <- list()  #resect current tour
          vrp$tours[[length(vrp$tours)+1]] <- tour

          for(p in 1:tour$stops){
            vi <- tour$stops.indices[[p]]
            r.index<-which(df$j == vi)
            if(li$log){
              cat("\tp:", p)
              cat("\ttour$stops:", tour$stops)
              cat("\tvi:", vi)
              cat("\tr.index:", r.index, "\n")
            }

            df[r.index, "position"] <-p
            df[r.index, "tour"] <-length(vrp$tours)
            df[r.index, "isEndCustomer"] <- FALSE
            if(p==1){
              df[r.index, "isEndCustomer"] <-  TRUE
            }else if(p==tour$stops){
              df[r.index, "isEndCustomer"] <-  TRUE
            } 

          }
          if(li$log) {
            cat("updated df:\n")
            print(df)
            cat("\t\tnew Tour: ", 
            sapply(tour$stops.list,function(o){o$label}),
            ".\n\t\t\tcosts:", tour$costs, "; loading:", tour$loading,"\n")  
          }
        } 
      } 
      vrp$x <- vrp$x*0
      for(t in length(vrp$tours):1){
          tour <- vrp$tours[[t]]
          if(length(tour) ==0){
            ndf<- df[,"tour"] >=t
            df[ndf,"tour"]<-df[ndf,"tour"] -1
            vrp$tours <- vrp$tours[-t]
          }else{
            indices <- tour$stops.indices
            for(e in 1:length(indices)){
              vi <- indices[[e]] +M
              if(e == length(indices)){
                vrp$x[vi,i] <- 1
              }
              if(e == 1){
                vrp$x[i,vi] <- 1
              }else{
                vj<-vi
                vi <- indices[[e-1]] +M
                vrp$x[vi,vj] <- 1
              }
            }
          }
      }
  		totalcosts <- totalcosts + vrp$totalcosts
  		if(li$log){
  			cat("\tTotalcosts for ",w$label,":",vrp$totalcosts ,"\n")
        cat("Finaler Tourenplan (neue Nummerierung):\n")
        print(df[order(df[,"tour"], df[,"position"]),])
  		}
  		w$vrp <- vrp
  		object$warehouses[[i]] <- w
  	}
  	if(li$log){
  		message(paste("Total costs are:", totalcosts))
  	}
  	return(object)
})
