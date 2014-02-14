setClass(
	Class = "HNUGeoSituation",
    representation=representation(
    	id          = "character",
        label       = "character",
    	nodes       = "list",
        links       = "list",
    	warehouses  = "list",
        customers   = "list",
        travelcosts = "numeric",
     transportcosts = "matrix",
     transportplan  = "matrix",
     shortestpath   = "list"
    ),
    prototype=prototype(
    	list(
    		id            = character(),
            label         = character(),
            nodes         = list(),
            warehouses    = list() ,
            links         = list(),
            customers     = list(),
            travelcosts   = numeric(),
        transportcosts    = matrix(),
        transportplan     = matrix(),
        shortestpath      = list()
    	)
    )
)
