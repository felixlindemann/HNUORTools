
#User-friendly creator
#
#
HNULink.create <- function(n1,n2,...) UseMethod("HNULink.create")
HNULink.create.default<-function(n1,n2,...){
    Link <- new("HNULink", n1,n2,...)
    
    return(Link)
}
 
setMethod("initialize", "HNULink", function(.Object, n1,n2,..., showwarnings=FALSE) {
      
    li <- list(...)
    costs<-1
    oneway <- FALSE
   
     
    if(is.null(n1))  stop("No Origin node is provided.")  
    if(is.null(n2))  stop("No Destination node is provided.")  
    
    # if(length(n1) !=1 ) stop("Incorrect Number of items of Origin-Node. Expected is one.")
    # if(length(n2) !=1 ) stop("Incorrect Number of items of Destination-Node. Expected is one.")

    if(!is.HNUNode(n1)) stop("The value for the origin Node is not of type HNUNode")
    if(!is.HNUNode(n2)) stop("The value for the destination Node is not of type HNUNode")

    .Object@origin     <- n1
    .Object@destination     <- n2
    
    if(!is.null(li$costs)) {
        if(length(li$costs) == 1) costs <- li$costs
    }
    if(!is.null(li$oneway)) {
        if(length(li$oneway) == 1) oneway <- li$oneway
    }
    if(is.null(li$used)) {
        if(length(li$used) == 1)   li$used <- FALSE
    }


    .Object@used <- used
    .Object@oneway <- oneway
    .Object@distance <- calc.Distance(n1,n2)
    .Object@costs <- calc.Distance(n1,n2,costs) 


    if(!is.null(li$label))    {
        
        .Object@label     <- as.character(li$label) 
    }   

     if(is.null(.Object@id) | length(.Object@id) == 0 ){
        .Object@id <- paste("n",sample(1:1000,1),sep="")
        w <- paste("Random ID (",.Object@id,") provided. Uniqueness may not be given.")
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
