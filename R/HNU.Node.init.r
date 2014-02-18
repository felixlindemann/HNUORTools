#User-friendly creator 

HNUNode.create <- function(...) UseMethod("HNUNode.create")
HNUNode.create.default<-function(...){
    node <- new("HNUNode", ...)
    # nodes <- get("nodes",envir=HNUORToolsEnv)
    # nodes[[length(nodes)+1]] <- node
    # assign("nodes",nodes,envir=HNUORToolsEnv)
    return(node)
}

setMethod("initialize", signature="HNUNode", function(.Object, data=NULL, ...) {
      
    li <- list(...)
    if(is.null(li$showwarnings))  li$showwarnings <- FALSE
    if(!is.null(data)){ 
        if(class(data) =="data.frame") {
            if(nrow(data) !=1) stop("Dataframes may only have one row for init.")
            li$x <- data$x
            li$y <- data$y
            li$id <- data$id
            li$label <- data$label 
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
    
    if(is.null(li$x)) {
        li$x <- as.numeric(runif(1,0,100)) 
        w <- paste("x-Coordinate simulated (x= ",li$x,").")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$x)!=1){
            stop("only 1 item for attribute x accepted.")
        } 
    } 
    if(is.null(li$y)) {
        li$y <- as.numeric(runif(1,0,100)) 
        w <- paste("y-Coordinate simulated (y= ",li$y,").")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$y)!=1){
            stop("only 1 item for attribute y accepted.")
        } 
    }

    if(is.null(li$id)) {
        li$id <- paste("n",sample(1:1000,1),sep="")
        w <- paste("Random ID (",li$id,") provided. Uniqueness may not be given.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$id)!=1){
            stop("only 1 item for attribute id accepted.")
        } 
    } 
    if(is.null(li$label)) {
        li$label <- li$id
        w <- paste("Random label (",li$label,") provided. Uniqueness may not be given.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$label)!=1){
            stop("only 1 item for attribute label accepted.")
        } 
    }

    .Object@x      <- as.numeric(  li$x)
    .Object@y      <- as.numeric(  li$y)
    .Object@id     <- as.character(li$id)
    .Object@label  <- as.character(li$label)
  
  
    if(validObject(.Object)) {
        return(.Object )
    }
})
