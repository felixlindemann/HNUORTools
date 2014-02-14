
setClass(
	Class = "HNULink",
    representation=representation(
    	id     = "character",
        label  = "character",
    	origin   = "HNUNode",
    	destination     = "HNUNode",
        costs = "numeric",
        distance = "numeric",
        oneway = "logical",
        used = "logical"
    ),
    prototype=prototype(
    	list(
    		id    = character(),
            label = character(),
            origin     = NA,
            destination     = NA,
            costs = numeric(),
            distance = numeric(),
            oneway = logical() ,
            used = logical()
    	)
    )
)
