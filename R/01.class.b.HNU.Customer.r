#' @exportClass Customer
#' @name Customer 
#' @rdname Customer
#' @aliases Customer-class 
#' @title The Customer class
#' 
#'  @description
#' This class is part of the \pkg{HNUORTools}. It extends the \code{\link{Node}} 
#' with attributes used for describing customer in an
#' \dfn{Operations-Research (OR)}-context.
#' 
#' @details Find here the defined slots for this class.
#' @section Slots defined: 
#' \describe{
#'    \item{\code{demand}:}{
#'      Object of class \code{"numeric"}, only non-negative Values are allowed.
#'     
#'      \strong{The default value is} a random value using 
#'      \code{as.numeric(sample(1:1000,1))}.
#'    }
#'    \item{\code{isDummy}:}{
#'      Object of class \code{"logical"}. 
#'      Indicates if this is an algorithmic-created \code{\link{Customer}}.
#'
#'
#'      \strong{The default value is} \code{FALSE}.
#'
#'      
#'      Some problems (e.g. the \dfn{Transportation-Problem (TPP)} 
#'      can be solved only, if the sum of demand equals the sum of supply.
#'      For this reason, some algorithms create virtual \code{\link{Customer}s}
#'      or \code{\link{Warehouse}s}.
#'    } 
#'  }
#' @section Slots defined in parent class (\code{\link{Node}}): 
#' \describe{
#'    \item{\code{id}:}{
#'      Object of class \code{"character"}, containing data from id.
#'      \strong{Should be unique}.
#'      The default value will be caluclated randomly.
#'    }
#'    \item{\code{label}:}{
#'      Object of class \code{"character"}, containing the label of 
#'      the \code{\link{Node}}.
#'      The default value will be caluclated randomly.
#'    }
#'    \item{\code{x}:}{
#'      Object of class \code{"numeric"}, containing the x-coordinate 
#'      of the \code{\link{Node}}.
#'      The default value will be caluclated randomly.
#'    }
#'    \item{\code{y}:}{
#'      Object of class \code{"numeric"}, containing the y-coordinate 
#'      of the \code{\link{Node}}.
#'      The default value will be caluclated randomly.
#'    }
#'  } 
#'       @section Creating objects of type \code{\link{Customer}}:
#'          \describe{
#'              \item{Creating an \code{S4-Object}}{ 
#'                  \code{new("Customer", ...)}
#'              } 
#'              \item{Converting from a \code{data.frame}}{ 
#'                  \code{as.Customer{<data.frame>}}
#'                  See also below in the Methods-Section.
#'              }
#'              \item{Converting from a \code{list}}{ 
#'                  \code{as.Customer{<list>}}
#'                  See also below in the Methods-Section.
#'              }
#'          }
#' @section Methods:
#' \describe{
#'              \item{\code{as.list(obj, ...)}}{
#'                  Converts a \code{\link{Customer}} into a \code{\link{list}}.
#'                  \code{...} are user-defined (not used) parameters.
#'              }
#'              \item{\code{as.data.frame(obj, ...)}}{
#'                  Converts a \code{\link{Customer}} into a \code{\link{data.frame}}.
#'                  \code{as.data.frame} accepts the optional parameter \code{withrownames} of class \code{"logical"} (default is \code{TRUE}). 
#'                  If \code{withrownames == TRUE} the returned \code{\link{data.frame}} will recieve the \code{id} as rowname.
#'                  \code{...} are user-defined (not used) parameters.
#'
#'              }
#'              \item{\code{as.Customer(obj)}}{
#'                  Converts an object of class \code{\link{data.frame}} or of class \code{\link{list}} into a \code{\link{Customer}}.
#'              } 
#'              \item{\code{is.Customer(obj)}}{
#'                  Checks if the object \code{obj} is of type \code{\link{Customer}}.
#'              } 
#'    \item{\code{\link{getDistance}}}{
#'      Calculating the distance between two \code{\link{Node}s}.
#'    }
#'    \item{\code{\link{getpolar}}}{
#'      Calculating the angle to the x-Axis of a link, 
#'      connecting two \code{\link{Node}s}.
#'    }
#'  }  
#' @section Derived from Classes:
#' \describe{
#'    \item{\code{\link{Node}}}{
#'      This class extends the \code{\link{Node}}-Class 
#'      with attributes for according to customers for OR-Problems.
#'    } 
#'  }
#' @section To be used for:
#' \describe{
#'    \item{Transportation-Problem (TPP)}{
#'      Finding a transportation-plan with e.g.
#'      North-West-Corner Rule (\code{\link{TPP.NW}}), 
#'      Column-Minimum-Method (\code{\link{TPP.CMM}}),
#'      Matrix-Minimum-Method (\code{\link{TPP.MMM}}), .
#'      Stepping-Stone-Method (\code{\link{TPP.SteppingStone}}) or
#'      MODI-Method (\code{\link{TPP.MODI}}).
#'    } 
#'    \item{Warehouse-Location-Problem (WLP)}{
#'      using the ADD-Algorithm (\code{\link{WLP.ADD}}). 
#'    }
#'    \item{Vehicle-Routing-Problem (VRP)}{
#'      using the Sweep-Algorithm (\code{\link{VRP.SWEEP}}) or
#'      Savings-Algorithm (\code{\link{VRP.SAVINGS}}). 
#'    }
#'  } 
#' @seealso \code{\link{TPP.NW}},\code{\link{TPP.CMM}},\code{\link{TPP.MMM}},
#'           \code{\link{TPP.SteppingStone}},\code{\link{TPP.MODI}},
#'           \code{\link{WLP.ADD}},\code{\link{VRP.SWEEP}},
#'           \code{\link{VRP.SAVINGS}},\code{\link{getDistance}},
#'           \code{\link{Node}}, \code{\link{Warehouse}}
##' @note 
#'      for citing use: Felix Lindemann (2014). HNUORTools: Operations Research Tools. R package version 1.1-0. \url{http://felixlindemann.github.io/HNUORTools/}.
#'      
#' @author Dipl. Kfm. Felix Lindemann \email{felix.lindemann@@hs-neu-ulm.de} 
#' 
#' Wissenschaftlicher Mitarbeiter
#' Kompetenzzentrum Logistik
#' Buro ZWEI, 17
#'
#' Hochschule fur angewandte Wissenschaften 
#' Fachhochschule Neu-Ulm | Neu-Ulm University 
#' Wileystr. 1 
#' 
#' D-89231 Neu-Ulm 
#' 
#' 
#' Phone   +49(0)731-9762-1437 
#' Web      \url{www.hs-neu-ulm.de/felix-lindemann/} 
#'          \url{http://felixlindemann.blogspot.de}
#' @examples  
#' # demo(HNUNode01)
#' # demo(HNUNode02)
#' # demo(HNUNode03)  
#' # Creating objects of class Customer:
#' x <- new("Customer")
#' 
#' # Doing it the proper way:
#' x <- new("Customer", x=2, y=3, id="c1", label="customer1", demand=10)
#' 
#' # Deriving from a data.frame
#' df<- data.frame(x=2, y=3, id="c1", label="customer1", demand=10)
#' x <- new("Customer", df)
#' 
#' # Using a user-friendly-function: 
#' # x <- new("Customer",x=2, y=3, id="c1", ...)
#'
setClass(
    Class="Customer",
    representation=representation(
        demand="numeric",
            isDummy = "logical"
    ),
    prototype=prototype(
        list(
            demand =numeric(),
            isDummy = logical()
        )
    ),
    contains="Node",
    validity = function(object){ 
        if( sum(is.null(object@demand)) + sum( is.na(object@demand)) > 0 ) {
            return(paste("Error with value demand: Value is not initialized", class(object@demand)))
        } else if( class(object@demand)!="numeric" ) {
            return(paste("Error with value demand: expected numeric datatype, but obtained", class(object@demand)))
        } else if( object@demand < 0  ) {
            return(paste("Error with value demand: expected numeric non negative value, but obtained", object@demand))
        } else{
            ## validity tests are not applied recursively by default,
            ## so this object is created (invalidly)
            return(validObject(new("Node", as.data.frame(object))))
       } 
    } 

)
################################### initialize - Method ###################################################
#' @title initialize Method
#' @name initialize
#' @aliases initialize,Customer-method 
#' @rdname initialize-methods 
setMethod("initialize", "Customer", function(.Object, data=NULL, ... ) {
      
    li <- list(...)
    if(is.null(li$showwarnings))  li$showwarnings <- FALSE
   
    if(!is.null(data)){ 
        if(class(data) =="data.frame") {
            if(nrow(data) !=1) stop("Dataframes may only have one row for init.")
            li$demand <- data$demand 
            li$isDummy <- data$isDummy 
        } else if(class(data) =="list") {
            
            duplicate.fields <- NULL
            for(j in 1:length(data)){
                l <- which(names(data)[j]==names(li))
                if(length(l) > 0){
                    duplicate.fields <- c(duplicate.fields,names(li)[l])
                }
            }
            duplicate.fields <- unique(duplicate.fields) 
            if(length(duplicate.fields)>0)  
                stop(paste("Cannot Construct Object. The field(s) '",
                        paste( 
                            duplicate.fields, 
                            collapse="', '", 
                            sep=""
                        ),
                        "' are given more than once.", 
                        sep=""
                     )
                )

            li<- append( data,li) 
        } else{ 
            stop("Error: argument data should be of type 'data.frame' or 'list'!")
        }
    }
    if(is.null(li$isDummy)) {
        li$isDummy <- FALSE 
        w <- paste("Attribute isDummy set to default: FALSE.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$isDummy)!=1){
            stop("only 1 item for attribute isDummy accepted.")
        } 
    }
     if(is.null(li$demand))    {
        li$demand <-   as.numeric(sample(1:1000,1))
        w <- paste("Random demand (",li$demand,") provided.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$demand)!=1){
            stop("only 1 item for attribute demand accepted.")
        } 
    }  

    .Object@isDummy <- li$isDummy
    .Object@demand     <- as.numeric(li$demand) 
    
    #get initalizer for Node
    .Object<-callNextMethod(.Object,data,...)

    if(validObject(.Object)) {
        return(.Object )
    }
})
################################### extract - Method ###################################################
#' @title Extract Method
#' @name $
#' @aliases $,Customer-method 
#' @rdname extract-methods 
setMethod("$","Customer",function(x,name) {return(slot(x,name))})
################################### Set - Method ###################################################
#' @title Set Method 
#' @name $<-
#' @aliases $<-,Customer-method 
#' @rdname set-methods
setMethod("$<-","Customer",function(x,name,value) {
  slot(x,name,check=TRUE) <- value
  valid<-validObject(x)
  return(x)
}) 
################################### Show - Method ###################################################
#' @title show Method
#' @description Object can be of type \code{\link{Customer}} 
#' @name show
#' @aliases show,Customer-method 
#' @param object Object can be of type \code{\link{Customer}} 
#' @rdname show-methods 
setMethod ("show", "Customer", function(object){
        cat("S4 class Customer:")
         
        if(!is.null(object@demand)  ) {
            cat("\tdemand: ", object@demand,"\n")
        } 
        cat("########### properties for  Node ################\n")
        callNextMethod(object)
        cat("####################################################\n")
    }
) # end show method
################################### as... - Method ###################################################

as.Customer.list = function(x, ...){return(new("Customer", x))}
as.Customer.data.frame = function(x, ...){return(new("Customer", x))}
as.data.frame.Customer = function(x, ...){
    li<-list(...)
    if(is.null(li$withrownames)) li$withrownames <- FALSE
    df<-    data.frame(id=x@id, label = x@label, x= x@x, y= x@y, demand = x@demand)
    if(li$withrownames) rownames(df)<-x@id
    return (df)
}
as.list.Customer = function(x, ...){
    list(id=x@id, label = x@label, x= x@x, y= x@y, demand = x@demand)
}
 

setGeneric("as.Customer",   function(x, ...) standardGeneric( "as.Customer")) 
setGeneric("is.Customer",   function(x, ...) standardGeneric( "is.Customer")) 
  
setMethod("as.Customer",     signature(x = "list"),       as.Customer.list) 
setMethod("as.Customer",    signature(x = "data.frame"),  as.Customer.data.frame) 
 
setMethod("as.list",        signature(x = "Customer"),        as.list.Customer) 
setMethod("as.data.frame",  signature(x = "Customer"),        as.data.frame.Customer) 


setAs("data.frame", "Customer", def=function(from){
    return(as.Customer.data.frame(from))
})

setAs("list", "Customer", def=function(from){
    return(as.Customer.list(from))
})
################################### is... - Method ###################################################
setMethod( "is.Customer", "Customer", function(x, ...){return(is(x ,"Customer"))})


