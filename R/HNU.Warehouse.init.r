
#User-friendly creator
#
#
HNUWarehouse.create <- function(...) UseMethod("HNUWarehouse.create")
HNUWarehouse.create.default<-function(...){
    Warehouse <- new("HNUWarehouse", ...)
    
    return(Warehouse)
}
 
setMethod("initialize", "HNUWarehouse", function(.Object, ..., showwarnings=FALSE) {
      
     li <- list(...)
     open <- TRUE
   
    if(length(li) == 1){ 
       if(  class(li[[1]]) == "data.frame"  )  { # data.frame
            df<- li[[1]]
            if(is.null(nrow(df))){
                w<-paste("No Input provided. Random content will be provided.")
                if(showwarnings) warning(w)
            } else {
 
                if(!is.null(df$supply))       .Object@supply     <- as.numeric(df$supply) 
                if(!is.null(df$fixcosts))       .Object@fixcosts     <- as.numeric(df$fixcosts) 
            }
        }
    }
    if(is.null(li$isDummy)) li$isDummy <- FALSE
    .Object@isDummy <- li$isDummy
    
    
    if(!is.null(li$supply))    {
        if(length(li$supply)!=1){
            stop("only 1 item for attribute supply accepted.")
        }
        .Object@supply     <- as.numeric(li$supply) 
    }  
    if(!is.null(li$open))    {
        if(length(li$open)!=1){
            stop("only 1 item for attribute supply accepted.")
        }

        open     <- as.logical(li$open) 
    }   
    .Object@open <- open
    if(!is.null(li$fixcosts))    {
        if(length(li$fixcosts)!=1){
            stop("only 1 item for attribute fixcosts accepted.")
        }
        .Object@fixcosts     <- as.numeric(li$fixcosts) 
    }   
    if(is.null(.Object@supply) | length(.Object@supply) == 0 ){
        .Object@supply <- as.numeric(sample(1:1000,1))
        w <- paste("Random supply (",.Object@supply,") provided.")
        if(showwarnings) warning(w) 
    }   
    if(is.null(.Object@fixcosts) | length(.Object@fixcosts) == 0 ){
        .Object@fixcosts <- as.numeric(sample(1:1000,1))
        w <- paste("Random fixcosts (",.Object@fixcosts,") provided.")
        if(showwarnings) warning(w) 
    }   
    
    #get initalizer for HNUNode
    .Object<-callNextMethod(.Object,...)

    if(validObject(.Object)) {
        return(.Object )
    }
})
