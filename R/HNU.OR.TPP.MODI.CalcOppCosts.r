setGeneric("HNU.OR.TPP.MODI.CalcOppCosts",  function(object,...)  standardGeneric("HNU.OR.TPP.MODI.CalcOppCosts") )
 setMethod("HNU.OR.TPP.MODI.CalcOppCosts", signature(object="HNUGeoSituation"),
  function(object,...){ 
  	li <- list(...) 
  	if(is.null(li$itermax))  li$itermax  <- -1
  	if(is.null(li$iter)) 	 li$iter 	 <- 1
  	if(is.null(li$log)) 	 li$log 	 <- TRUE
  	if(is.null(li$plot)) 	 li$plot 	 <- TRUE
    tmpI <- 0

    if(li$log) message("\t\tHNU.OR.TPP.MODI.CalcOppCosts\n")
    
    cij <- object$tpp.costs
    x <- object$tpp.x
  
  	opp<-HNU.OR.getInitialMatrix(object, initialvalue=NA, ...)
    N<- length(object$customers)
    M<- length(object$warehouses)

    u<- rep(NA, M)
    v<- rep(NA, N)
    u[1] <- 0
    #
    CONTINUE <- TRUE
    xRichtung <- FALSE 
    # Die Ermittlung des Polygonzuges ist ein Wechsel aus der 
    # Suche in X-Richtung und der Suche in y-Richtung.
    while(CONTINUE) {
      CONTINUE <- FALSE

      # use formula
      # u(i) <- cij - vj
      # v(j) <- cij - ui
      if(xRichtung){
        for(j in 1:N){
          if(is.na(v[j])){
            for(i in 1:M){
              if(x[i,j] > 0 & !is.na(u[i])){
                v[j] <- cij[i,j] - u[i]
                break
              }
            }
          }
        } 
      }else{
        for(i in 1:M){
          if(is.na(u[i])){
            for(j in 1:N){
              if(x[i,j] >0 & !is.na(v[j])){
                u[i] <- cij[i,j] - v[j]
                break
              }
            }
          }
        } 
      }  
      # are all variables set?
      for(i in 1:M){
        if(is.na(u[i])){
          CONTINUE  <- TRUE
          break
        }
      }
      if(!CONTINUE){
        for(j in 1:N){
          if(is.na(v[j])){
            CONTINUE  <- TRUE
            break
          }
        }
      } 
      tmpI <- tmpI + 1
      xRichtung <- !xRichtung # next time other wa
      if(tmpI >= M*N +1) break
      if(!CONTINUE) break
    }
    # fill opp-costs matrix
    for(i in 1:M){
      for(j in 1:N){
        if(x[i,j] == 0 ){
          opp[i,j] <- cij[i,j] - u[i] - v[j]
        }
      }
    }        
    
    if(li$log){
      message("\t\tOpp Costs (MODI-Methode)\n")
      print(opp)
      message("\t\tu[i]\n")
      print(u)
      message("\t\tv[j]\n")
      print(v)
    }        
        
    object$tpp.costs.opp <- opp

	return(object)
  }  
)