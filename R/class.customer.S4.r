
setClass(
	Class="HNUCustomer",
    representation=representation( 
    	demand="numeric",
        location="HNUNode"
    ),
    prototype=prototype(
    	list(
    		demand=NA,
            location=NA
    	)
    ), # Defines default value (optional)
    validity=function(object) { 
    	# Can be defined in a separate step using setValidity
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
        } else if( !validObject(object@location)){ 
            return(paste("Error with location: ", validObject(object@location)))
        } else{ 
            return(TRUE)
        }
    }
) 
setMethod("initialize", "HNUCustomer", function(.Object, location,x,y,demand,label="",id="") { 
    if(missing(location)){
        .Object@location <-  new("HNUNode", x=x, y=y, label=label, id=id)        
    } else{
        if(class(location)[1] == "HNUNode"){
            .Object@location <- location
        } else{
            simpleError(paste("Error with location. Object expected of type HNUNode, but obtained", class(location)))
        }
    }
    if(missing(demand)){demand <- 0 } # No demand -> if not provided
    .Object@demand <- demand
    
     if(validObject(.Object)) {return(.Object )}
    
})
