
setClass(
	Class="HNUWarehouse",
    representation=representation( 
    	supply="numeric",
        fixcosts="numeric",
        location="HNUNode"
    ),
    prototype=prototype(
    	list(
            supply=NA,
            fixcosts=NA,
            location=NA
    	)
    ), # Defines default value (optional)
    validity=function(object) { 
    	# Can be defined in a separate step using setValidity
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
        } else if( !validObject(object@location)){ 
            return(paste("Error with location: ", validObject(object@location)))
        } else{ 
            return(TRUE)
        }
    }
) 
setMethod("initialize", "HNUWarehouse", function(.Object, location,x,y,supply,fixcosts,label="",id="") { 
    if(missing(location)){
        .Object@location <-  new("HNUNode", x=x, y=y, label=label, id=id)        
    } else{
        if(class(location)[1] == "HNUNode"){
            .Object@location <- location
        } else{
            simpleError(paste("Error with location. Object expected of type HNUNode, but obtained", class(location)))
        }
    }
    if(missing(supply)){supply <- 2*10^9 } # integer max
    .Object@supply <- supply
    if(missing(fixcosts)){fixcosts <- 0} # no FixCosts in this case
    .Object@fixcosts <- fixcosts
     if(validObject(.Object)) {return(.Object )}
    
})
