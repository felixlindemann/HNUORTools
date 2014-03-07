
# Set validity Check
.Customer.valid <- function(object){ 
               if( sum(is.null(object@demand)) + sum( is.na(object@demand)) > 0 ) {
            return(paste("Error with value demand: Value is not initialized", class(object@demand)))
        } else if( class(object@demand)!="numeric" ) {
            return(paste("Error with value demand: expected numeric datatype, but obtained", class(object@demand)))
        } else if( object@demand < 0  ) {
            return(paste("Error with value demand: expected numeric non negative value, but obtained", object@demand))
        }  
        else{
            ## validity tests are not applied recursively by default,
            ## so this object is created (invalidly)
            return(validObject(new("Node", as.data.frame(object))))
       }
}
setValidity("Customer", .Customer.valid)
