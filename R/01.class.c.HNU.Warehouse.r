#' @exportClass Warehouse
#' @name Warehouse 
#' @rdname Warehouse
#' @aliases Warehouse-class 
#' @title The Warehouse class
#' 
#'  @description
#' This class is part of the \pkg{HNUORTools}. It extends the \code{\link{Node}} 
#' with attributes used for describing customer in an
#' \dfn{Operations-Research (OR)}-context.
#' 
#' @details Find here the defined slots for this class.
#' @section Slots defined: 
#' \describe{
#'    \item{\code{supply}:}{
#'      Object of class \code{"numeric"}, only non-negative Values are allowed.
#'     
#'      \strong{The default value is} a random value using 
#'      \code{as.numeric(sample(1:1000,1))}.
#'    }
#'    \item{\code{fixcosts}:}{
#'      Object of class \code{"numeric"}, only non-negative Values are allowed.
#'     
#'      \strong{The default value is} a random value using 
#'      \code{as.numeric(sample(1:1000,1))}.
#'    }
#'    \item{\code{open}:}{
#'      Object of class \code{"logical"}. In a \dfn{Warehouse-Location-Problem (WLP)}
#'      this will indicate if the \code{\link{Warehouse}}  is used or not.
#' 
#'      \strong{The default value is} \code{TRUE}. 
#'    } 
#'    \item{\code{VRP}:}{
#'      Object of class \code{"list"}. In the current version of
#'      \pkg{HNUORTools} the solution to a \dfn{Vehicle Routing Problem (VRP)} 
#'      will be stored as a list. (See Sweep-Algorithm (\code{\link{VRP.SWEEP}}) or
#'      Savings-Algorithm (\code{\link{VRP.SAVINGS}}) for detailled information.)
#'    } 
#'    \item{\code{isDummy}:}{
#'      Object of class \code{"logical"}. 
#'      Indicates if this is an algorithmic-created \code{\link{Warehouse}}. 
#'
#'      \strong{The default value is} \code{FALSE}. 
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
#'       @section Creating objects of type \code{\link{Warehouse}}:
#'          \describe{
#'              \item{Creating an \code{S4-Object}}{ 
#'                  \code{new("Warehouse", ...)}
#'              } 
#'              \item{Converting from a \code{data.frame}}{ 
#'                  \code{as.Warehouse{<data.frame>}}
#'                  See also below in the Methods-Section.
#'              }
#'              \item{Converting from a \code{list}}{ 
#'                  \code{as.Warehouse{<list>}}
#'                  See also below in the Methods-Section.
#'              }
#'          }
#'      @section Methods:
#' \describe{
#'              \item{\code{as.list(obj, ...)}}{
#'                  Converts a \code{\link{Warehouse}} into a \code{\link{list}}.
#'                  \code{...} are user-defined (not used) parameters.
#'              }
#'              \item{\code{as.data.frame(obj, ...)}}{
#'                  Converts a \code{\link{Warehouse}} into a \code{\link{data.frame}}.
#'                  \code{as.data.frame} accepts the optional parameter \code{withrownames} of class \code{"logical"} (default is \code{TRUE}). 
#'                  If \code{withrownames == TRUE} the returned \code{\link{data.frame}} will recieve the \code{id} as rowname.
#'                  \code{...} are user-defined (not used) parameters.
#'
#'              }
#'              \item{\code{as.Warehouse(obj)}}{
#'                  Converts an object of class \code{\link{data.frame}} or of class \code{\link{list}} into a \code{\link{Warehouse}}.
#'              } 
#'              \item{\code{is.Warehouse(obj)}}{
#'                  Checks if the object \code{obj} is of type \code{\link{Warehouse}}.
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
#' @note 
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
#' # Creating objects of class Warehouse:
#' x <- new("Warehouse")
#' 
#' # Doing it the proper way:
#' x <- new("Warehouse", x=2, y=3, id="w1", label="Warehouse1", supply=10, fixcosts = 20)
#' 
#' # Deriving from a data.frame
#' df<- data.frame(x=2, y=3, id="w1", label="Warehouse1", supply=10, fixcosts = 20)
#' x <- new("Warehouse", df)
#' 
#' # Using a user-friendly-function: 
#' # x <- new("Warehouse",x=2, y=3, id="w1", ...)
#'
setClass(
    Class="Warehouse",
    representation=representation(
        supply="numeric",
        fixcosts="numeric",
        open="logical",
        isDummy="logical",
        vrp = "list"
    ),
    prototype=prototype(
        list(
            supply =numeric(),
            fixcosts = numeric (),
            open = logical(),
            isDummy = logical(),
            vrp = list()
        )
    ),
    contains="Node",
    validity = function(object){ 
        if( sum(is.null(object@fixcosts)) + sum( is.na(object@fixcosts)) > 0 ) {
            return(paste("Error with value fixcosts: Value is not initialized", class(object@fixcosts)))
        } else if( class(object@fixcosts)!="numeric" ) {
            return(paste("Error with value fixcosts: expected numeric datatype, but obtained", class(object@fixcosts)))
        } else if( object@fixcosts < 0  ) {
            return(paste("Error with value fixcosts: expected numeric non negative value, but obtained", object@fixcosts))
        } else if( sum(is.null(object@supply)) + sum( is.na(object@supply)) > 0 ) {
            return(paste("Error with value supply: Value is not initialized", class(object@supply)))
        } else if( class(object@supply)!="numeric" ) {
            return(paste("Error with value supply: expected numeric datatype, but obtained", class(object@supply)))
        } else if( object@supply < 0  ) {
            return(paste("Error with value supply: expected numeric non negative value, but obtained", object@supply))
        }  
        else{
            ## validity tests are not applied recursively by default,
            ## so this object is created (invalidly)
            return(validObject(new("Node", as.data.frame(object))))
       }
    }
)
################################### initialize - Method ###################################################
#' @title initialize Method
#' @name initialize
#' @aliases initialize,Warehouse-method 
#' @rdname initialize-methods 
setMethod("initialize", "Warehouse", function(.Object, data=NULL, ...) {
      
    li <- list(...)

    if(is.null(li$showwarnings)) li$showwarnings <- FALSE 
    

    if(!is.null(data)){ 
        if(class(data) =="data.frame") {
            if(nrow(data) !=1) stop("Dataframes may only have one row for init.")
            li$supply <- data$supply
            li$fixcosts <- data$fixcosts
            li$isDummy <- data$isDummy
            li$open <- data$open 
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
    if(is.null(li$open))  {
        li$open <- TRUE
        w <- paste("Attribute Open set to default: TRUE.")
        if(li$showwarnings) warning(w) 
    } else  {
        if(length(li$open)!=1){
            stop("only 1 item for attribute supply accepted.")
        } 
    }   
    if(is.null(li$supply))    {
        li$supply <-   as.numeric(sample(1:1000,1))
        w <- paste("Random supply (",li$supply,") provided.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$supply)!=1){
            stop("only 1 item for attribute supply accepted.")
        } 
    }  
    if(is.null(li$fixcosts)){
        li$fixcosts <- as.numeric(sample(1:1000,1))
        w <- paste("Random fixcosts (",li$fixcosts,") provided.")
        if(li$showwarnings) warning(w) 
    }else{
        if(length(li$fixcosts)!=1){
            stop("only 1 item for attribute fixcosts accepted.")
        }
    }    
    .Object@isDummy <- li$isDummy
    .Object@supply     <- li$supply
    .Object@open     <- as.logical(li$open) 
    .Object@fixcosts     <- as.numeric(li$fixcosts) 
    .Object@supply <- as.numeric(li$supply) 
     
    #get initalizer for Node
    .Object<-callNextMethod(.Object,data,...)

    if(validObject(.Object)) {
        return(.Object )
    }else{
         stop("No valid Object was created.")
    }
})
################################### Show - Method ###################################################
#' @title show Method
#' @name show
#' @description Object can be of type \code{\link{Warehouse}} 
#' @param object Object can be of type \code{\link{Warehouse}} 
#' @aliases show,Warehouse-method 
#' @rdname show-methods 
setMethod ("show", "Warehouse", function(object){
        cat("S4 class Warehouse:")
         
        if(!is.null(object@supply)  ) {
            cat("\tsupply: ", object@supply,"\n")
        } 
        if(!is.null(object@fixcosts)  ) {
            cat("\tfixcosts: ", object@fixcosts,"\n")
        } 
        if(!is.null(object@open)  ) {
            cat("\topen: ", object@open,"\n")
        } 
        cat("########### properties for  Node ################\n")
        callNextMethod(object)
        cat("####################################################\n")
    }
) # end show method
################################### as... - Method ###################################################
as.Warehouse.list = function(x, ...){return(new("Warehouse", x))}
as.Warehouse.data.frame = function(x, ...){return(new("Warehouse", x))}
as.data.frame.Warehouse = function(x, ...){
    li<-list(...)
    if(is.null(li$withrownames)) li$withrownames <- FALSE
    df<-data.frame(id=x@id, label = x@label, x= x@x, y= x@y, fixcosts = x@fixcosts, supply = x@supply, open = x@open)
    if(li$withrownames) rownames(df)<-x@id
    return (df)
}
as.list.Warehouse = function(x, ...){
    list(id=x@id, label = x@label, x= x@x, y= x@y, fixcosts = x@fixcosts, supply = x@supply, open = x@open)
}
 
setGeneric("as.Warehouse",  function(x, ...) standardGeneric( "as.Warehouse")) 
setGeneric("is.Warehouse",  function(x, ...) standardGeneric( "is.Warehouse")) 

 
setMethod("as.Warehouse",     signature(x = "list"),      as.Warehouse.list) 
setMethod("as.Warehouse",   signature(x = "data.frame"),  as.Warehouse.data.frame) 
 
setMethod("as.list",        signature(x = "Warehouse"),       as.list.Warehouse) 
setMethod("as.data.frame",  signature(x = "Warehouse"),       as.data.frame.Warehouse) 


setAs("data.frame", "Warehouse", def=function(from){
    return(as.Warehouse.data.frame(from))
})

setAs("list", "Warehouse", def=function(from){
    return(as.Warehouse.list(from))
})

################################### is... - Method ###################################################
setMethod( "is.Warehouse", "Warehouse", function(x, ...){return(is(x ,"Warehouse"))})
 
