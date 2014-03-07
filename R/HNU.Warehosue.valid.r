
# Set validity Check
.Warehouse.valid <- function(object){ 
               if( sum(is.null(object@fixcosts)) + sum( is.na(object@fixcosts)) > 0 ) {
            return(paste("Error with value fixcosts: Value is not initialized", class(object@fixcosts)))
        } else if( class(object@fixcosts)!="numeric" ) {
            return(paste("Error with value fixcosts: expected numeric datatype, but obtained", class(object@fixcosts)))
        } else if( object@fixcosts < 0  ) {
            return(paste("Error with value fixcosts: expected numeric non negative value, but obtained", object@fixcosts))
        } else if( sum(is.null(object@supply)) + sum( is.na(object@supply)) > 0 ) {
            return(paste("Error with value supply: Value is not initialized", class(object@supply)))
        } else if( class(object@supply)!="numeric" ) {
            return(paste("Error with value supply: expected numeric datatype, but obtained", class(object@supply)))
        } else if( object@supply < 0  ) {
            return(paste("Error with value supply: expected numeric non negative value, but obtained", object@supply))
        }  
        else{
            ## validity tests are not applied recursively by default,
            ## so this object is created (invalidly)
            return(validObject(new("Node", as.data.frame(object))))
       }
}
setValidity("Warehouse", .Warehouse.valid)
