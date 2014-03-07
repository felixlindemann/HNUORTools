 
setMethod("initialize", "GeoSituation", function(.Object, ... ) {
      
    li <- list(...)
    .Object@nodes <- list()
    .Object@warehouses <- list()
    .Object@customers <- list()
    .Object@links <- list()

    if(is.null(li$showwarnings))  li$showwarnings <- FALSE
   
    if(!is.null(li$nodes)) .Object@nodes <- li$nodes
    if(!is.null(li$warehouses)) .Object@warehouses <- li$warehouses
    if(!is.null(li$customers)) .Object@customers <- li$customers
    if(!is.null(li$links)) .Object@links <- li$links
    
    .Object@tpp.x <- matrix() # will no be assignable on init.
    .Object@tpp.costs <- matrix() # will no be assignable on init.
    .Object@tpp.costs.opp <- matrix() # will no be assignable on init.

    
    if(!is.null(li$id)){
        .Object@id      <- as.character(  li$id)
    }
    if(!is.null(li$label)){
        .Object@label      <- as.character(  li$label)
    }
    if(is.null(.Object@id) | length(.Object@id) == 0 ){
        .Object@id <- paste("n",sample(1:1000,1),sep="")
        w <- paste("Random ID (",.Object@id,") provided. Uniqueness may not be given.")
        if(li$showwarnings) warning(w) 
    }       
    if(is.null(.Object@label) | length(.Object@label) == 0 ){
        .Object@label <- .Object@id 
        w <- paste("No label provided. ID (id = ",.Object@id,") used instead.")
        if(li$showwarnings) warning(w)
    } 
  
    if(validObject(.Object)) {
        return(.Object )
    }
})
