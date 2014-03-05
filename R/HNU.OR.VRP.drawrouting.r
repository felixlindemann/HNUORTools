setGeneric("HNU.OR.VRP.drawrouting",  function(object,...)  standardGeneric("HNU.OR.VRP.drawrouting") )
 setMethod("HNU.OR.VRP.drawrouting", signature(object="HNUGeoSituation"),
  function(object,...){ 
    message("HNU.OR.VRP.drawrouting\n")
  	li <- list(...)  
    ################################################################################
    # parameters:
    #     - drawroutingtowarehouse: 
    #         defines if connection to warehouse is drawn
    #         default is true
    #     - routingcolors: 
    #         defines the colors of the connection for each warehouse.
    #         default is 1
    #         it's recommended to use the same colors as used for the 
    #         warehouses
    # used functions, that accept optional values:
    #     - arrows
    #         - attributes x1,y1,col are blocked
    ################################################################################
    
    #check if is VRP
  	M<- length(object$warehouses)
  	if(M <=1) stop("This is not a VRP - no warehouse has been found.")
    # set default values
    if(is.null(li$drawroutingtowarehouse)) li$drawroutingtowarehouse <- TRUE
    if(is.null(li$routingcolors)) li$routingcolors = 1
    if(length(li$routingcolors)!=M) li$routingcolors <- rep(li$routingcolors, M)

    #iterate over all warehouses (multiwarehouse vrp.)
    for(m in 1:M){
      #get Current Warehouse
      w <- object$warehouses[[m]]
      #select drawing color
      routingcolor <- li$routingcolors[m]
      # if tours exists
      if(length(w$vrp$tours)>0){
        #iterate over all tours
        for(t in 1:length(w$vrp$tours)){
          #select current tour
          tour <- w$vrp$tours[[t]]

          #get the number of stops
          n<-length(tour$stops.list)
          
          if (n==0){
            # do nothing - if no stops are assigned. 
          }else if(n ==1){    
            # if it's a "pendeltour", draw both ways
            # code = 3 doesn't work for generality reasons,
            # if code=3 is assigned as default, 
            # users will not be able to define the arrows-code-attribute
            # without getting errors.
            n1<- tour$stops.list[[1]]    
            arrows(w$x, w$y, x1 = n1$x, y1 = n1$y, col=routingcolor, ...) 
            arrows(n1$x, n1$y, x1 = w$x, y1 = w$y, col=routingcolor, ...)     
          } else{
            # if more than one stop is assigned
            for(i in 1:(n-1)){
              # get first stop
              n1<- tour$stops.list[[i]] 

              if(li$drawroutingtowarehouse & i==1){ 
                # draw connection of depot, 
                # if this variable is set
                # draw the connection from the depot to the first stop
                  arrows(w$x, w$y, x1 = n1$x, y1 = n1$y, col=routingcolor, ...) 
                # draw the connection from the last stop to depot
                  n2 <- tour$stops.list[[n]]
                  arrows(n2$x, n2$y, x1 = w$x, y1 = w$y, col=routingcolor, ...)  
              }  
              # get following node of n1 and draw the connection
              n2<-tour$stops.list[[i+1]]               
              arrows(n1$x, n1$y, x1 = n2$x, y1 = n2$y, col=routingcolor, ...)                
            }  
          }
        }
      }  
    }
  }
)
 