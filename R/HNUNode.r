setClass(
	Class = "HNUNode",
    representation=representation(
    	id     = "character",
    	x      = "numeric",
    	y      = "numeric",
    	label  = "character"
    ),
    prototype=prototype(
    	list(
    		id    = character(),
            x     = numeric(),
            y     = numeric(),
            label = character() 
    	)
    )
)

.HNUNode.valid <- function(object){
               if( sum(is.null(object@x)) + sum( is.na(object@x)) > 0 ) {
            return(paste("Error with value x: Value is not initialized", class(object@x)))
        } else if( sum(is.null(object@y)) + sum( is.na(object@y)) > 0) {
            return(paste("Error with value y: Value is not initialized", class(object@y)))
        } else if( class(object@x)!="numeric" ) {
            return(paste("Error with value x: expected numeric datatype, but obtained", class(object@x)))
        } else if( class(object@y)!="numeric" ) {
            return(paste("Error with value y: expected numeric datatype, but obtained", class(object@y)))
        } else if( length(object@x)!=1 ){ 
            return(paste("Error with value x: expected numeric data of length 1, but obtained", length(object@x), " --> no arrays supported here"))
        } else if( length(object@y)!=1 ){ 
            return(paste("Error with value y: expected numeric data of length 1, but obtained", length(object@y), " --> no arrays supported here"))
        } else{ 
            return(TRUE)
        }
}
setValidity("HNUNode", .HNUNode.valid)
is.HNUNode <- function(x){is(x,"HNUNode")}
setAs(
    "HNUNode", 
    "data.frame", 
    function(from) {
        df <- data.frame()
        tryCatch(
            {
            df <- data.frame(
                id      = from@id,
                label   = from@label, 
                x       = from@x, 
                y       = from@y
            )
            return(df)
            }, warning = function(w) {
                cat(paste("Create Dataframe from Node: Unerwartete Warnung:\n", w)) 
            }, error   = function(e) {
                cat(paste("Create Dataframe from Node: Unerwarteter Fehler:\n", e)) 
                stop(e)
            }, finally = { 
                return(df)
            }
        )
    }
)
setAs(
    "data.frame", 
    "HNUNode", 
    function(from) {
        o <- NA
        tryCatch(
            { 
                tmp.id = character()
                tmp.label = character()
                tmp.x = numeric()
                tmp.y = numeric()

                if(!is.null(from$id)){
                    tmp.id <- from$id
                }
                if(!is.null(from$label)){
                    tmp.label <- from$label
                }
                if(!is.null(from$x)){
                    tmp.x <- from$x
                }
                if(!is.null(from$y)){
                    tmp.y <- from$y
                }

                o <- HNUNode(x = tmp.x,y = tmp.y, id = tmp.id, label = tmp.label)
     
            }, warning = function(w) {
                cat(paste("Create Node from Dataframe: Unerwartete Warnung:\n", w)) 
            }, error   = function(e) {
                cat(paste("Create Node from Dataframe: Unerwarteter Fehler:\n", e)) 
                stop(e)
            }, finally = { 
                return(o)
            } 
        )
    }
)

# Userfriendly function
HNUNode <- function(x = numeric(),y = numeric(), id = character(), label = character()){
    tryCatch(
        {
            o <- NA
            if(missing(id) | length(id) == 0 ){
                id <- paste("n",sample(1:1000,1),sep="")
            }
            if(missing(x) | length(x) == 0 ){
                x<-runif(1,0,100)
            }
            if(missing(y) | length(y) == 0 ){
                y<-runif(1,0,100)
            }
            if(missing(label) | length(label) == 0 ){
                label <- id
            }  
            o <- new("HNUNode", id = id, x = x, y = y, label = label) 
            isvalid <- validObject(o) 
        }, warning = function(w) {
            cat(paste("Create Node: Unerwartete Warnung:\n", w)) 
        }, error   = function(e) {
            cat(paste("Create Node: Unerwarteter Fehler:\n", e)) 
            stop(e)
        }, finally = { 
            return(o)
        } 
    ) 
} 
