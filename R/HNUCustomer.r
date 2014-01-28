
setClass(
    Class="HNUCustomer",
    representation=representation(
        demand="numeric"
    ),
    prototype=prototype(
        list(
            demand =numeric()
        )
    ),
    contains="HNUNode"
)

.HNUCustomer.valid <- function(object){
    if( sum(is.null(object@demand)) + sum( is.na(object@demand)) > 0 ) {
        return(paste("Error with value demand: demand is not initialized", class(object@demand)))
    } else if(class(object@demand)!="numeric") {
        return(paste("Error with value demand: expected numeric datatype, but obtained", class(object@demand))) 
    } else if(length(object@demand)!=1){ 
        return(paste("Error with value demand: expected numeric data of length 1, but obtained", 
              length(object@demand), " --> no arrays supported here"))         
    } else if(object@demand<0){ 
        return(paste("Error with value demand: expected is a non negative value, but obtained", (object@demand), 
                     " --> no negative values supported here"))         
    } else{ 
        return(TRUE)
    }
} 
setValidity("HNUCustomer", .HNUCustomer.valid)
is.HNUCustomer <- function(x){is(x,"HNUCustomer")}


# Userfriendly function
HNUCustomer <- function(x = numeric(), y = numeric(), demand = numeric(), id = character(), label = character()){
    tryCatch(
        {
            o <- NA
            if(missing(demand)){
                demand <- as.numeric( sample(1:1000,1) )
            }
            n <- HNUNode( id = id, x = x, y = y, label = label)  
            o <- new("HNUCustomer", id = n@id, x = n@x, y = n@y, demand = demand, label = n@label) 
            isvalid <- validObject(o) 
        }, warning = function(w) {
            cat(paste("Create Customer: Unerwartete Warnung:\n", w)) 
        }, error   = function(e) {
            cat(paste("Create Customer: Unerwarteter Fehler:\n", e)) 
            stop(e)
        }, finally = { 
            return(o)
        } 
    ) 
}  

