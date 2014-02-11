
setClass(
    Class="HNUCustomer",
    representation=representation(
        demand="numeric"
    ),
    prototype=prototype(
        list(
            demand =numeric()
        )
    ),
    contains="HNUNode"
)

#to-String(Method)
setMethod ("show", "HNUCustomer", function(object){
        cat("S4 class HNUCustomer:")
         
        if(!is.null(object@demand)  ) {
            cat("\tdemand: ", object@demand,"\n")
        } 
        cat("########### properties for HNU Node ################\n")
        callNextMethod(object)
        cat("####################################################\n")
    }
) # end show method
