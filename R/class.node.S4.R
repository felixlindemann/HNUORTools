setClass(
	Class="HNUNode",
    representation=representation(
    	id="character",
    	x="numeric",
    	y="numeric",
    	label="character"
    ),
    prototype=prototype(
    	list(
    		id=paste("Node", -1), # Fake Random ID
    		x =runif(1,0,100), # random x 
    		y =runif(1,0,100), # random y
    		label = NA 
    	)
    ), # Defines default value (optional)
    validity=function(object) { 
    	# Can be defined in a separate step using setValidity
    		   if( sum(is.null(object@x)) + sum( is.na(object@x)) > 0 ) {
            return(paste("Error with value x: Value is not initialized", class(object@x)))
        } else if( sum(is.null(object@y)) + sum( is.na(object@y)) > 0) {
            return(paste("Error with value y: Value is not initialized", class(object@y)))
        } else if(class(object@x)!="numeric") {
            return(paste("Error with value x: expected numeric datatype, but obtained", class(object@x)))
        } else if(class(object@y)!="numeric") {
            return(paste("Error with value y: expected numeric datatype, but obtained", class(object@y)))
        } else if(length(object@x)!=1){ 
           	return(paste("Error with value x: expected numeric data of length 1, but obtained", length(object@x), " --> no arrays supported here"))
        } else if(length(object@y)!=1){ 
           	return(paste("Error with value y: expected numeric data of length 1, but obtained", length(object@y), " --> no arrays supported here"))
        } else{ 
            return(TRUE)
        }
    }
)
setMethod("initialize", "HNUNode", function(.Object, x,y,label="",id="") { 
    if(missing(x)){x<- runif(1,0,100)} # random x 
    if(missing(y)){y<- runif(1,0,100)} # random x 
    if(missing(id)){id<-paste("Node", sample(1:100000,1))}
    if(missing(label)){label<-id}
    .Object@x  <- x
    .Object@y  <- y
    .Object@id  <- id
    .Object@label  <- label

    if(validObject(.Object)) {return(.Object )}
})
