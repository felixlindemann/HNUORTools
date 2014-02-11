
# Set validity Check
.HNUNode.valid <- function(object){
               if( sum(is.null(object@x)) + sum( is.na(object@x)) > 0 ) {
            return(paste("Error with value x: Value is not initialized", class(object@x)))
        } else if( sum(is.null(object@y)) + sum( is.na(object@y)) > 0) {
            return(paste("Error with value y: Value is not initialized", class(object@y)))
        } else if( class(object@x)!="numeric" ) {
            return(paste("Error with value x: expected numeric datatype, but obtained", class(object@x)))
        } else if( class(object@y)!="numeric" ) {
            return(paste("Error with value y: expected numeric datatype, but obtained", class(object@y)))
        } else if( length(object@x)!=1 ){ 
            return(paste("Error with value x: expected numeric data of length 1, but obtained", length(object@x), ": no arrays supported here"))
        } else if( length(object@y)!=1 ){ 
            return(paste("Error with value y: expected numeric data of length 1, but obtained", length(object@y), ": no arrays supported here"))
        } else{ 
            return(TRUE)
        }
}
setValidity("HNUNode", .HNUNode.valid)
