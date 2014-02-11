
# Set validity Check
.HNULink.valid <- function(object){


        if( sum(is.null(object@origin)) + sum( is.na(object@origin)) > 0 ) {
            return(paste("Error with value origin: Value is not initialized", class(object@origin)))
        } 
        if( sum(is.null(object@destination)) + sum( is.na(object@destination)) > 0) {
            return(paste("Error with value destination: Value is not initialized", class(object@destination)))
        } 
        if( !is.HNUNode(object@origin) ) {
            return(paste("Error with value origin: expected HNUNode, but obtained", class(object@origin)))
        } 
        if( !is.HNUNode(object@destination) ) {
            return(paste("Error with value destination: expected HNUNode, but obtained", class(object@destination)))
        } 
        if( length(object@distance  ) != 1 ){ 
            return(paste("Error with value distance: expected value of length 1, but obtained", length(object@distance)))
        } 
        if( length(object@costs  ) != 1 ){ 
            return(paste("Error with value costs: expected value of length 1, but obtained", length(object@costs)))
        } 
        
        if( object@distance < 0 ){ 
            return(paste("Error with value distance: expected non-negative value, but obtained", object@distance))
        } 
        if( object@costs < 0 ){ 
            return(paste("Error with value costs: expected non-negative value, but obtained", object@costs))
        } 
        return(TRUE)
         
}
setValidity("HNULink", .HNULink.valid)
