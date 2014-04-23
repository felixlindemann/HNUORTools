#' @aliases getDistance,numeric,numeric-method 
#' @rdname getDistance  
#' @export
#' @examples
#' # getDistance(c(1,1),c(4,5),costfactor=1) # should result 5. 
#' # getDistance(c(1,1),c(4,5),costfactor=2) # should result 10. 
setMethod( "getDistance", signature =c("numeric", "numeric"),
  function(n0=c(0,0),n1,...){
    li<- list(...)
    
    if(is.null(li$costfactor)) li$costfactor <-1 
    dist <- sqrt( (n1[1] - n0[1])^2 + (n1[2] - n0[2])^2 ) 
    
    
    if(!is.null(li$digits)){
      if(class(li$digits) != "numeric"){
        warning(paste("Parameter digits is ignored, as the wrong class is provided. Expected is numeric, provided:", class(li$digits)))
      }else{
        if(li$digits<0){
          warning(paste("Parameter digits is ignored. Only non-negative values are accepted. Provided value:", li$digits))
        }else{
          dist <- round(dist, digits = li$digits)
        }
      }
    }
    dist <- dist * li$costfactor
    return(dist)       
  }
) 











