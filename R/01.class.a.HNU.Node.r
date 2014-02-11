setClass(
	Class = "HNUNode",
    representation=representation(
    	id     = "character",
        label  = "character",
    	x      = "numeric",
    	y      = "numeric"
    ),
    prototype=prototype(
    	list(
    		id    = character(),
            label = character(),
            x     = numeric(),
            y     = numeric() 
    	)
    )
)

 
#to-String(Method)
setMethod ("show", "HNUNode", function(object){
        cat("S4 class HNUNode:")
        if(!is.null(object@id)) {
            cat("\tid:",object@id)
        }
        cat("\n\tisS4:  ",isS4(object),"\n")
        if(!is.null(object@label)) {
            cat("\tlabel: ",object@label, "\n")
        }
        if(!is.null(object@x) && !is.null(object@y)) {
            cat("\t(x/y): ","(",object@x,"/",object@y,")\n")
        } 
    }
) # end show method
