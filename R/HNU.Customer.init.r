
#User-friendly creator
#
#
HNUCustomer.create <- function(...) UseMethod("HNUCustomer.create")
HNUCustomer.create.default<-function(...){
    customer <- new("HNUCustomer", ...)
    
    return(customer)
}
 
setMethod("initialize", "HNUCustomer", function(.Object, ..., showwarnings=FALSE) {
      
     li <- list(...)

   
    if(length(li) == 1){ 
       if(  class(li[[1]]) == "data.frame"  )  { # data.frame
            df<- li[[1]]
            if(is.null(nrow(df))){
                w<-paste("No Input provided. Random content will be provided.")
                if(showwarnings) warning(w)
            } else {
 
                if(!is.null(df$demand))       .Object@demand     <- as.numeric(df$demand) 
            }
        }
    }
    if(!is.null(li$demand))    {
        if(length(li$demand)!=1){
            stop("only 1 item for attribute demand accepted.")
        }
        .Object@demand     <- as.numeric(li$demand) 
    }   
    if(is.null(.Object@demand) | length(.Object@demand) == 0 ){
        .Object@demand <- as.numeric(sample(1:1000,1))
        w <- paste("Random demand (",.Object@demand,") provided.")
        if(showwarnings) warning(w) 
    }   
    
    #get initalizer for HNUNode
    .Object<-callNextMethod(.Object,...)

    if(validObject(.Object)) {
        return(.Object )
    }
})
