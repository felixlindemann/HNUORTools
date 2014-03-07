 
setMethod("initialize", "Warehouse", function(.Object, data=NULL, ...) {
      
    li <- list(...)

    if(is.null(li$showwarnings)) li$showwarnings <- FALSE 
    

    if(!is.null(data)){ 
        if(class(data) =="data.frame") {
            if(nrow(data) !=1) stop("Dataframes may only have one row for init.")
            li$supply <- data$supply
            li$fixcosts <- data$fixcosts
            li$isDummy <- data$isDummy
            li$open <- data$open 
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
    if(is.null(li$open))  {
        li$open <- TRUE
        w <- paste("Attribute Open set to default: TRUE.")
        if(li$showwarnings) warning(w) 
    } else  {
        if(length(li$open)!=1){
            stop("only 1 item for attribute supply accepted.")
        } 
    }   
    if(is.null(li$supply))    {
        li$supply <-   as.numeric(sample(1:1000,1))
        w <- paste("Random supply (",li$supply,") provided.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$supply)!=1){
            stop("only 1 item for attribute supply accepted.")
        } 
    }  
    if(is.null(li$fixcosts)){
        li$fixcosts <- as.numeric(sample(1:1000,1))
        w <- paste("Random fixcosts (",li$fixcosts,") provided.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$fixcosts)!=1){
            stop("only 1 item for attribute fixcosts accepted.")
        }
    }    
    .Object@isDummy <- li$isDummy
    .Object@supply     <- li$supply
    .Object@open     <- as.logical(li$open) 
    .Object@fixcosts     <- as.numeric(li$fixcosts) 
    .Object@supply <- as.numeric(li$supply) 
     
    #get initalizer for Node
    .Object<-callNextMethod(.Object,data,...)

    if(validObject(.Object)) {
        return(.Object )
    }else{
         stop("No valid Object was created.")
    }
})
