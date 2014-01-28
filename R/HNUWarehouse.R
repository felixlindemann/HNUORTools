
setClass(
    Class="HNUWarehouse",
    representation=representation(
        supply="numeric",
        fixcosts="numeric"
    ),
    prototype=prototype(
        list(
            supply =numeric(),
            fixcosts = numeric()
        )
    ),
    contains="HNUNode"
)

.HNUWarehouse.valid <- function(object) {  
           if( sum(is.null(object@fixcosts)) + sum( is.na(object@fixcosts)) > 0 ) {
        return(paste("Error with value fixcosts: fixcosts is not initialized", class(object@fixcosts)))
    } else if(class(object@fixcosts)!="numeric") {
        return(paste("Error with value fixcosts: expected numeric datatype, but obtained", class(object@fixcosts))) 
    } else if(length(object@fixcosts)!=1){ 
        return(paste("Error with value fixcosts: expected numeric data of length 1, but obtained", 
              length(object@fixcosts), " --> no arrays supported here"))         
    } else if(object@fixcosts<0){ 
        return(paste("Error with value fixcosts: expected is a non negative value, but obtained", (object@fixcosts), 
                     " --> no negative values supported here"))         
    } else if( sum(is.null(object@supply)) + sum( is.na(object@supply)) > 0 ) {
        return(paste("Error with value supply: supply is not initialized", class(object@supply)))
    } else if(class(object@supply)!="numeric") {
        return(paste("Error with value supply: expected numeric datatype, but obtained", class(object@supply))) 
    } else if(length(object@supply)!=1){ 
        return(paste("Error with value supply: expected numeric data of length 1, but obtained", 
              length(object@supply), " --> no arrays supported here"))         
    } else if(object@supply<0){ 
        return(paste("Error with value supply: expected is a non negative value, but obtained", (object@supply), 
                     " --> no negative values supported here"))
    } else {
        return(TRUE)
    }
}

setValidity("HNUWarehouse", .HNUWarehouse.valid)
is.HNUWarehouse <- function(x){is(x,"HNUWarehouse")}


# Userfriendly function
HNUWarehouse <- function(x = numeric(), y = numeric(), supply = numeric(), fixcosts = numeric(), id = character(), label = character()){
    tryCatch(
        {
            o <- NA
            if(missing(supply) | length(supply) != 1){
                supply <- as.numeric( sample(1:1000,1) )
            }
            if(missing(fixcosts) | length(fixcosts) != 1){
                fixcosts <- 0 
            }
            n <- HNUNode( id = id, x = x, y = y, label = label)  
            o <- new("HNUWarehouse", id = n@id, x = n@x, y = n@y, supply = supply, fixcosts = fixcosts, label = n@label) 
            isvalid <- validObject(o) 
        }, warning = function(w) {
            cat(paste("Create Warehouse: Unerwartete Warnung:\n", w)) 
        }, error   = function(e) {
            cat(paste("Create Warehouse: Unerwarteter Fehler:\n", e)) 
            stop(e)
        }, finally = { 
            return(o)
        } 
    ) 
}  

