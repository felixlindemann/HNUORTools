#' @aliases getDistance,Node,Node-method 
#' @rdname getDistance  
#' @examples
#' # w1 <- new("Warehouse", x=0, y=0)
#' # c1 <- new("Customer",  x=3, y=4)
#' # getDistance(w1,c1) # should result 5.  
#' # getDistance(w1,c1,costfactor=3) # should result 15.  
setMethod( "getDistance", signature = c("Node", "Node"),
    function(n0,n1,   ...) {
        li<-list(...)
 
        validObject(n1)
        validObject(n0)
        
        if(is.null(li$costfactor)) li$costfactor <-1

        dist <- sqrt( (n1@x - n0@x)^2 + (n1@y - n0@y)^2 ) * li$costfactor
        return(dist)
    }
)
  
#' @aliases getpolar,Node,Node-method 
#' @rdname getpolar   
#' @examples
#' # w1 <- new("Warehouse", x=0, y=0)
#' # c1 <- new("Customer",  x=1, y=1)
#' # getpolar(w1,c1,deg=TRUE) # should result 45.  
setMethod("getpolar", signature=c("Node", "Node"),
    function(n0,n1,   ...) {
        li<-list(...) 
        validObject(n0)
        validObject(n1)
        
        p0<-c(n0$x,n0$y)
        p1<-c(n1$x,n1$y)

        value <- getpolar(p0,p1, ...)
        
        return(value)
    }
)
