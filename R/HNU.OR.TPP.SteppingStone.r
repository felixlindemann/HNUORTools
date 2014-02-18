setGeneric("HNU.OR.TPP.SteppingStone",  function(object,...)  standardGeneric("HNU.OR.TPP.SteppingStone") )
 setMethod("HNU.OR.TPP.SteppingStone", signature(object="HNUGeoSituation"),
  function(object,...){ 
    message("HNU.OR.TPP.SteppingStone\n")
  	li <- list(...) 
  	if(is.null(li$itermax))  li$itermax  <- -1
  	if(is.null(li$iter)) 	 li$iter 	 <- 1
  	if(is.null(li$log)) 	 li$log 	 <- TRUE
  	if(is.null(li$plot)) 	 li$plot 	 <- TRUE

  	M<- length(object$customers)
 	N<- length(object$warehouses)

	cij <- object$tpp.costs

	while( m.opp < 0 ){
		#get indices of m.opp
		if(li$log) { message(paste("\tentering iteration:", li$iter,"\n"))}
		x <-   object$tpp.x 
		if(length(x[x>0]) != M+N -1 ){
			msg<-paste("The current solution is not a base-solution. -> ",
				 "the amount of base-variables is not as expected M+N-1 == length(x[x>0])",
				 "(",(M+N-1),"!= ",length(x[x>0]),").\n",
				 "This problem is not solveable with the current version of HNUORTools.")
			stop(msg)
		}

		oldcosts <- sum(x*cij)
		object<-HNU.OR.TPP.SteppingStone.CalcOppMatrix(object, ...)
		opp <- object$tpp.costs.opp
		m.opp <- min(opp, na.rm = TRUE)  
		if(length(m.opp) == 0 | m.opp > 0 ) {
			message("optimal solution found after ",li$iter," iteration(s).")
			message("total Costs are: ", oldcosts)
			break
		}
		Element<-which( opp==m.opp, arr.ind=T )[1,]
		i <- Element[1]
		j <- Element[2]
		Poly<-HNU.OR.TPP.SteppingStone.GetBestPolygonZug(object,i=i,j=j,...)# returns
		# HNU.OR.TPP.SteppingStone.GetBestPolygonZug
		# n.Poly<-NULL
		# n.Poly$List<-data.frame(i=i.i,j=j.j)
		# n.Poly$opp<-cij[i.i,j.j]
		# n.Poly$vertical<-TRUE 
		# n.Poly$closed<-FALSE 
		# n.Poly$NextIsAdd<-FALSE
		# n.Poly$count<-1 
		# n.Poly$closed=TRUE
		if(li$log){
			message(paste("\t\tPolygonzug: for Element (",rownames(opp)[i],"/",colnames(opp)[j],")"))
			cat("\t\t\tOpportunity Costs:",Poly$opp," \n") 
			cat("\t\t\tCircle: ") 
			for(k in 1:nrow(Poly$List)){
				cat("(",rownames(opp)[Poly$List$i[k]],"/",
						 colnames(opp)[Poly$List$j[k]],")-")
			}
			k<-1
			cat("(",rownames(opp)[Poly$List$i[k]],"/",
						 colnames(opp)[Poly$List$j[k]],")\n")
		}
		# get Maximum amount to be excanged
		dmin<-max(x)*2 +10 #set upper bound 
		s <- seq(2,length(Poly$List[,2]),by=2)	# define sequence to search
		for(l in s){
			k<-c(Poly$List[l,1],Poly$List[l,2]) # get indices 
			i <- k[1]
			j <- k[2]
			dmin <- max( min(dmin, x[i,j]), 0) # make sure, no negative values are met.
 
		} 
		# do the exchange
		for(l in seq(1,length(Poly$List[,2]))){
			k<-c(Poly$List[l,1],Poly$List[l,2]) # get indices 
			i <- k[1]
			j <- k[2]
			x[i,j]<- x[i,j] + dmin * (-1)^(l+1) # subtract from every second pivot element 
		}
		newcosts <- sum(x*cij)
		if(li$log){
			
				  cat("\t\t\tExchange amount :",dmin," \n") 
				  cat("\t\t\tNew Total Costs :",newcosts,"- improved by ",(oldcosts-newcosts)," (= ",Poly$opp,"*",dmin,") in comparisson to prior iteration.\n") 
			cat(paste("\t\tNew Transportation Plan: \n"))
			print(x)
			cat("------------------- End of Iteration", li$iter, " ------------------- \n\n")
		}
		
		object$tpp.x <- x 
		li$iter <- li$iter+1
		if(li$itermax >0 & li$itermax< li$iter){
			message("Abort by User definition after ",li$itermax," iteration(s).")	
			break
		} 
	}
	return(object)
  }  
)