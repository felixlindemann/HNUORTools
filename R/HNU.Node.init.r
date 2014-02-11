#User-friendly creator 

HNUNode.create <- function(...) UseMethod("HNUNode.create")
HNUNode.create.default<-function(...){
    node <- new("HNUNode", ...)
    # nodes <- get("nodes",envir=HNUORToolsEnv)
    # nodes[[length(nodes)+1]] <- node
    # assign("nodes",nodes,envir=HNUORToolsEnv)
    return(node)
}

# setGeneric("HNUNode.create",function(...){standardGeneric("HNUNode.create")})

# setMethod("HNUNode.create", signature(), HNUNode.create)


setMethod("initialize", "HNUNode", function(.Object, ..., showwarnings=FALSE) {
      
    li <- list(...)

   
    if(length(li) == 1){ 
       if(  class(li[[1]]) == "data.frame"  )  { # data.frame
            df<- li[[1]]
            if(is.null(nrow(df))){
                w<-paste("No Input provided. Random content will be provided.")
                if(showwarnings) warning(w)
            } else {
 
                if(!is.null(df$id))       .Object@id     <- as.character(df$id)
                if(!is.null(df$label))    .Object@label  <- as.character(df$label)
                if(!is.null(df$x))        .Object@x      <- as.numeric(  df$x)
                if(!is.null(df$y))        .Object@y      <- as.numeric(  df$y)

            }
        }
    }
    if(!is.null(li$x)){
        .Object@x      <- as.numeric(  li$x)
    }
    if(!is.null(li$y)){
        .Object@y      <- as.numeric(  li$y)
    }
    
    if(!is.null(li$id)){
        .Object@id      <- as.character(  li$id)
    }
    if(!is.null(li$label)){
        .Object@label      <- as.character(  li$label)
    }
    if(is.null(.Object@id) | length(.Object@id) == 0 ){
        .Object@id <- paste("n",sample(1:1000,1),sep="")
        w <- paste("Random ID (",.Object@id,") provided. Uniqueness may not be given.")
        if(showwarnings) warning(w) 
    }
    if(is.null(.Object@x) | length(.Object@x) == 0 ){
        .Object@x <-as.numeric(runif(1,0,100))
        w <- paste("x-Coordinate simulated (x= ",.Object@x,").")
        if(showwarnings) warning(w)
    }               
    if(is.null(.Object@y) | length(.Object@y) == 0 ){
        .Object@y <-as.numeric(runif(1,0,100))
        w <- paste("y-Coordinate simulated (y= ",.Object@y,").")
        if(showwarnings) warning(w)
    }               
    if(is.null(.Object@label) | length(.Object@label) == 0 ){
        .Object@label <- .Object@id 
        w <- paste("No label provided. ID (id = ",.Object@id,") used instead.")
        if(showwarnings) warning(w)
    } 
  
    if(validObject(.Object)) {
        return(.Object )
    }
})
