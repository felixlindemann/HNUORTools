setGeneric("HNU.OR.VRP.drawrouting",  function(object,...)  standardGeneric("HNU.OR.VRP.drawrouting") )
 setMethod("HNU.OR.VRP.drawrouting", signature(object="HNUGeoSituation"),
  function(object,...){ 
    message("HNU.OR.VRP.drawrouting\n")
  	li <- list(...) 



  	M<- length(object$warehouses)
  	if(M <=1) stop("This is not a VRP - no warehouse has been found.")
    if(is.null(li$drawroutingtowarehouse)) li$drawroutingtowarehouse <- TRUE
    if(is.null(li$routingcolors)) li$routingcolors = 1

    if(length(li$routingcolors)!=M) li$routingcolors <- rep(li$routingcolors, M)

    for(m in 1:M){
      w <- object$warehouses[[m]]
      routingcolor <- li$routingcolors[m]
      for(t in 1:length(w$vrp$tours)){
        tour <- w$vrp$tours[[t]]

        n<-length(tour$stops.list)
         
        if (n==0){

        }else if(n ==1){    
          n1<- tour$stops.list[[1]]    
          arrows(w$x, w$y, x1 = n1$x, y1 = n1$y, col=routingcolor, ...) 
          arrows(n1$x, n1$y, x1 = w$x, y1 = w$y, col=routingcolor, ...)     
        } else{
          for(i in 1:(n-1)){
            n1<- tour$stops.list[[i]] 
            if(li$drawroutingtowarehouse & i==1){ 
                arrows(w$x, w$y, x1 = n1$x, y1 = n1$y, col=routingcolor, ...) 
                n2 <- tour$stops.list[[n]]
                arrows(n2$x, n2$y, x1 = w$x, y1 = w$y, col=routingcolor, ...)  
            }  
            n2<-tour$stops.list[[i+1]]
             
            arrows(n1$x, n1$y, x1 = n2$x, y1 = n2$y, col=routingcolor, ...) 
             
           }  
        }
      }  
    }
  }
)
 