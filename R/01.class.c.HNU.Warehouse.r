
setClass(
    Class="HNUWarehouse",
    representation=representation(
        supply="numeric",
        fixcosts="numeric"
    ),
    prototype=prototype(
        list(
            supply =numeric(),
            fixcosts = numeric ()
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
        cat("########### properties for HNU Node ################\n")
        callNextMethod(object)
        cat("####################################################\n")
    }
) # end show method
