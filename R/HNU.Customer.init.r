
#User-friendly creator
#
#
HNUCustomer.create <- function(...) UseMethod("HNUCustomer.create")
HNUCustomer.create.default<-function(...){
    customer <- new("HNUCustomer", ...)
    
    return(customer)
}
 
setMethod("initialize", "HNUCustomer", function(.Object, data=NULL, ... ) {
      
    li <- list(...)
    if(is.null(li$showwarnings))  li$showwarnings <- FALSE
   
    if(!is.null(data)){ 
        if(class(data) =="data.frame") {
            if(nrow(data) !=1) stop("Dataframes may only have one row for init.")
            li$demand <- data$demand 
            li$isDummy <- data$isDummy 
        } else if(class(data) =="list") {
            
            duplicate.fields <- NULL
            for(j in 1:length(data)){
                l <- which(names(data)[j]==names(li))
                if(length(l) > 0){
                    duplicate.fields <- c(duplicate.fields,names(li)[l])
                }
            }
            duplicate.fields <- unique(duplicate.fields) 
            if(length(duplicate.fields)>0)  
                stop(paste("Cannot Construct Object. The field(s) '",
                        paste( 
                            duplicate.fields, 
                            collapse="', '", 
                            sep=""
                        ),
                        "' are given more than once.", 
                        sep=""
                     )
                )

            li<- append( data,li) 
        } else{ 
            stop("Error: argument data should be of type 'data.frame' or 'list'!")
        }
    }
    if(is.null(li$isDummy)) {
        li$isDummy <- FALSE 
        w <- paste("Attribute isDummy set to default: FALSE.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$isDummy)!=1){
            stop("only 1 item for attribute isDummy accepted.")
        } 
    }
     if(is.null(li$demand))    {
        li$demand <-   as.numeric(sample(1:1000,1))
        w <- paste("Random demand (",li$demand,") provided.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$demand)!=1){
            stop("only 1 item for attribute demand accepted.")
        } 
    }  

    .Object@isDummy <- li$isDummy
    .Object@demand     <- as.numeric(li$demand) 
    
    #get initalizer for HNUNode
    .Object<-callNextMethod(.Object,data,...)

    if(validObject(.Object)) {
        return(.Object )
    }
})
