
setClass(
    Class="HNUWarehouse",
    representation=representation(
        supply="numeric",
        fixcosts="numeric",
        open="logical",
        isDummy="logical"
    ),
    prototype=prototype(
        list(
            supply =numeric(),
            fixcosts = numeric (),
            open = logical(),
            isDummy = logical()
        )
    ),
    contains="HNUNode"
)

#to-String(Method)
setMethod ("show", "HNUWarehouse", function(object){
        cat("S4 class HNUWarehouse:")
         
        if(!is.null(object@supply)  ) {
            cat("\tsupply: ", object@supply,"\n")
        } 
        if(!is.null(object@fixcosts)  ) {
            cat("\tfixcosts: ", object@fixcosts,"\n")
        } 
        if(!is.null(object@open)  ) {
            cat("\topen: ", object@open,"\n")
        } 
        cat("########### properties for HNU Node ################\n")
        callNextMethod(object)
        cat("####################################################\n")
    }
) # end show method
