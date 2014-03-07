 
setMethod("initialize", "Link", function(.Object, n1,n2,... ) {
      
    li <- list(...)
    
    if(is.null(li$showwarnings))  li$showwarnings <- FALSE
   
     
    if(is.null(n1))  stop("No Origin node is provided.")  
    if(is.null(n2))  stop("No Destination node is provided.")  
    
    # if(length(n1) !=1 ) stop("Incorrect Number of items of Origin-Node. Expected is one.")
    # if(length(n2) !=1 ) stop("Incorrect Number of items of Destination-Node. Expected is one.")

    if(!is.Node(n1)) stop("The value for the origin Node is not of type Node")
    if(!is.Node(n2)) stop("The value for the destination Node is not of type Node")

    .Object@origin          <- n1
    .Object@destination     <- n2
    if(is.null(li$oneway)) {
        li$oneway <- FALSE 
        w <- paste("Attribute oneway set to default: FALSE.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$oneway)!=1){
            stop("only 1 item for attribute oneway accepted.")
        } 
    }
    if(is.null(li$used)) {
        li$used <- FALSE 
        w <- paste("Attribute used set to default: FALSE.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$used)!=1){
            stop("only 1 item for attribute used accepted.")
        } 
    }
    if(is.null(li$distance)) {
        li$distance <- calc.Distance(n1,n2)
        w <- paste("automatic distance (",li$distance,") calculated.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$distance)!=1){
            stop("only 1 item for attribute distance accepted.")
        } 
    }
    if(is.null(li$costs)) {
        if(is.null(li$costfactor)) {li$costfactor <- 1}
        else{
            if(length(li$costfactor)!=1){
                stop("only 1 item for attribute costfactor accepted.")
            } 
        }
        li$costs <- calc.Distance(n1,n2,li$costfactor) 
        w <- paste("automatic costs (",li$costs,") calculated.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$costs)!=1){
            stop("only 1 item for attribute costs accepted.")
        } 
    }
    if(is.null(li$id)) {
        li$id <- paste("l",sample(1:1000,1),sep="")
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

    .Object@used     <- as.logical(li$used)
    .Object@oneway   <- as.logical(li$oneway)
    .Object@distance <- as.numeric(li$distance)
    .Object@costs    <- as.numeric(li$costs)
    .Object@label    <- as.character(li$label) 
    .Object@id       <- as.character(li$id) 
    
 
    if(validObject(.Object)) {
        return(.Object )
    }
})
