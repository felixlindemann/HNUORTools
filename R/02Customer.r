#' @exportClass Customer
#' @name Customer 
#' @rdname Customer
#' @aliases Customer-class 
#' @title The Customer class
#'
#' @description 
#' This class is part of the \pkg{HNUORTools}. It represents 
#' the base class for every locateable class in an
#' \dfn{Operations-Research (OR)}-context.
#' 
#' @details Find here the defined slots for this class.
#'      @section Slots: 
#'        \describe{
#'              \item{\code{demand}:}{
#'                  Object of class \code{"numeric"}, containing the amount for the demand.
#'                  The default value cannot be negative.
#'              }
#'              \item{\code{isDummy}:}{
#'                  Object of class \code{"logical"}, indicating if a customer was added 
#'                  in an algorithm (e.g. TPP-Column-Minimum-Method) in order to avoid
#'                  degenerated soulutions.
#'              }
#'        }
#'      @section Slots (from \code{\link{Node}}): 
#'          \describe{
#'              \item{\code{id}:}{
#'                  Object of class \code{"character"}, containing data from id.
#'                  \strong{Should be unique}.
#'                  The default value will be caluclated randomly.
#'              }
#'              \item{\code{label}:}{
#'                  Object of class \code{"character"}, containing the label of 
#'                  the \code{\link{Customer}}.
#'                  The default value will be caluclated randomly.
#'              }
#'              \item{\code{x}:}{
#'                  Object of class \code{"numeric"}, containing the x-coordinate 
#'                  of the \code{\link{Customer}}.
#'                  The default value will be caluclated randomly.
#'              }
#'              \item{\code{y}:}{
#'                  Object of class \code{"numeric"}, containing the y-coordinate 
#'                  of the \code{\link{Customer}}.
#'                  The default value will be caluclated randomly.
#'              }
#'          }
#'
#' @seealso The classes are derived from this class and the following Methods can be used with this class.:
#'      @section Creating objects of type \code{\link{Customer}}:
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
#'      @section Methods:
#'          \describe{
#'              \item{\code{as.list(Customer, ...)}}{
#'                  Converts a \code{\link{Customer}} into a \code{\link{list}}.
#'                  \code{...} are user-defined (not used) parameters.
#'              }
#'              \item{\code{as.data.frame(Customer, ...)}}{
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
#'              \item{\code{\link{getDistance}}}{
#'                  Calculating the distance between two \code{\link{Node}s} 
#'                  (As the classes \code{\link{Customer}} and \code{\link{Warehouse}} 
#'                  depend on \code{\link{Node}}, distances can be calculated between any of these objects).
#'              }
#'              \item{\code{\link{getpolar}}}{
#'                  Calculating the polar-angle to the x-Axis of a link, 
#'                  connecting two \code{\link{Node}s} 
#'                  (As the classes \code{\link{Customer}} and \code{\link{Warehouse}} 
#'                  depend on \code{\link{Node}}, polar-angles can be calculated between any of these objects).
#'              }
#'          } 
#'   
#'      @section Derived Classes:
#'         None.
#'      @section To be used for:
#'          \describe{
#'              \item{WLP}{
#'                   Warehouse-Location-Problem
#'              }
#'              \item{TPP}{
#'                  Transportation Problem
#'              }
#'              \item{VRP}{
#'                  Vehicle Routing Problem
#'              }
#'          } 
#'    
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
#' # create a new Customer with specific values
#' x<- new("Customer", x= 10, y=20, id="myid", label = "mylabel", demand = 20)
#' x
#' # create from data.frame
#' df<- data.frame(x=10,y=20, demand = 30)
#' new("Customer", df)
#' as(df, "Customer")
#' as.Customer(df)
#' #create some Customers
#' n1 <- new("Customer",x=10,y=20, demand = 30, id ="n1")
#' n2 <- new("Customer",x=13,y=24, demand = 40, id ="n2")
#' # calculate Beeline distance
#' #getDistance(n1,n2) # should result 5
#' #getDistance(n1,n2, costs = 2) # should result 10
setClass(
  Class = "Customer",
  representation=representation(
    demand="numeric",
    isDummy = "logical"
  ), 
  contains = "Node",
  validity = function(object){
    
    
    df <- as.data.frame(object)
    node <- as.Node(df)
    if(validObject(node)){ 
      N <- length(object@id) 
      if(length(object@demand) != N) 
        return("Invalid Object of Type 'Customer': the length of the attributes 'id' and 'demand' differ!")
      if(length(object@isDummy) != N)     
        return("Invalid Object of Type 'Customer': the length of the attributes 'id' and 'isDummy' differ!")
       
      if( sum(is.null(object@demand)) + sum( is.na(object@demand)) > 0 ) 
        return(paste("Invalid Object of Type 'Customer': Error with value demand: Value is not initialized", class(object@demand)))
      
      if( sum(is.null(object@isDummy)) + sum( is.na(object@isDummy)) > 0) 
        return(paste("Invalid Object of Type 'Customer': Error with value isDummy: Value is not initialized", class(object@isDummy)))
      
      if( class(object@demand)!="numeric" ) 
        return(paste("Invalid Object of Type 'Customer': Error with value demand: expected numeric datatype, but obtained", class(object@demand)))
      
      if( class(object@isDummy)!="logical" ) 
        return(paste("Invalid Object of Type 'Customer': Error with value isDummy: expected logical datatype, but obtained", class(object@isDummy))) 
      
      if(min(object@demand)<0)
          return(paste("Invalid Object of Type 'Customer': Error with value demand: at least one value is negative. Only positive Values are allowed."))
      
      return(TRUE) 
      
    }else{
      return (FALSE)
    }
  } 
)
################################### initialize - Method ###################################################
#' @title initialize Method
#' @name initialize
#' @aliases initialize,Customer-method 
#' @rdname initialize-methods  
setMethod("initialize", signature="Customer", function(.Object, data=NULL, ...) {
  
  li <- list(...)
  if(is.null(li$showwarnings))  li$showwarnings <- FALSE
  
  #init from parent Node-Object.
  N<-1
  
  .Object<-callNextMethod(.Object,data,...) 
  if(!is.null(data)){ 
    
    if(class(data) =="list") {
      data <- as.data.frame(data)
    }
    if(class(data) =="data.frame") {
      N<-nrow(data)
      li$isDummy  <- data$isDummy
      li$demand   <- data$demand
      
    } else{ 
      stop("Error: argument data should be of type 'data.frame'!")
    }
  }  
  if(is.null(li$demand) | length(li$demand) ==0) {
    li$demand <- as.numeric(sample(30:100,N,replace=TRUE)) 
    w <- paste("y-Coordinate simulated (y= ",li$demand,").")
    if(li$showwarnings) warning(w) 
  } 
  if(is.null(li$isDummy)) {
    li$isDummy <- rep(FALSE,N)
  }
  
  .Object@demand  <- as.numeric(  li$demand)
  .Object@isDummy <- as.logical(  li$isDummy) 
  
  validObject(.Object)
  
  return(.Object ) 
})
################################### extract - Method ###################################################
#' @title Extract Methods
#' @name [
#' @aliases [,Customer-method 
#' @rdname Extract-methods-2 
setMethod("[", "Customer",
          function(x, i, j, drop){
            N <- length(x)
            if(min(i) <=0) stop("Index i out of bound. it must be positive non zero.")
            if(max(i) >N) stop("Index i out of bound. it must not be larger than the total length.")
            
            
            if(!missing(j)){
              if(class(j) == "character"){
                return(slot(x,j)[i])
              }
              if(class(j) == "integer"){
                return(slot(x,j)[i])   
              }
            }
            df <- as.data.frame(x)[i,]
            return (new("Customer", df))         
          } 
) 
################################### Set - Method ###################################################

#' @title Set Methods
#' @name [<-
#' @aliases [<-,Customer-method 
#' @rdname Set-methods-2
#' @param x The Customer
#' @param i row-Index
#' @param j Name or Column index
setReplaceMethod("[",signature(x="Customer"),
                 function(x,i,j,value){
                   N <- length(x)
                   if(min(i) <=0) stop("Index i out of bound. It must be positive non zero.")
                   if(max(i) >N)  stop("Index i out of bound. It must not be larger than the total length.")
                   if(length(value)!=length(i)) stop("The replacement length is not equal to the number of elements to be replaced.")    
                   
                   if(is(value, "Customer")){
                     
                      
                     #replace the item(s) regarding to i
                     x$id[i]    <- value@id 
                     x$label[i] <- value@label 
                     x$x[i]     <- value@x 
                     x$y[i]     <- value@y
                     x$demand[i]     <- value@demand
                     x$isDummy[i]     <- value@isDummy
                     
                     # return new item
                     return (x) 
                   } else {
                     if(missing(j)) {
                       # call the function recursivly
                       # might throw an error if the conversion is not supported.
                       x[i] <- as.Customer(value)
                     }else{ 
                       if(class(j) == "character"){
                         slot(x,j)[i]     <- value
                       }else if(class(j) == "integer" | class(j) == "numeric"){ 
                         slot(x,slotNames(x)[j])[i]  <- value   
                       }else{
                         warning(paste("no changes have been made. the provided value of j (class:",class(j), "value:",j,") was not recocnized."))
                       }
                     }
                     valid<-validObject(x)
                     return(x) 
                   }
                 } 
)

################################### as... - Method ###################################################
#local Methods
as.Customer.list       = function(x, ...){return(new("Customer", data=x))}
as.Customer.data.frame = function(x, ...){return(new("Customer", data=x))}
#local convert message
as.data.frame.Customer = function(x, ...){
  li<-list(...)
  if(is.null(li$withrownames)) li$withrownames <- FALSE
  df<-data.frame(id=x@id, label = x@label, x= x@x, y= x@y, demand=x@demand, isDummy = x@isDummy)
  if(li$withrownames) rownames(df)<-x@id
  return (df)
}
as.list.Customer = function(x, ...){list(id=x@id, label = x@label, x= x@x, y= x@y, demand=x@demand, isDummy = x@isDummy)}


#' @export
setGeneric("as.Customer",       function(x, ...) standardGeneric( "as.Customer")) 

#' @title Convert Objects to Customers
#' @name as.Customer  
#' @param x an  Object to convert.
#' @param ... optional parameters
#' @aliases as.Customer,list-method 
#' @rdname as.Customer-methods 
setMethod("as.Customer",     signature(x = "list"),       as.Customer.list) 

#' @title Convert Objects to Customers
#' @name as.Customer
#' @aliases as.Customer,data.frame-method 
#' @rdname as.Customer-methods 
setMethod("as.Customer",    signature(x = "data.frame"),  as.Customer.data.frame)  

#' @title Convert Objects to lists
#' @name as.list 
#' @aliases as.list,Customer-method 
#' @rdname as.list-methods 
setMethod("as.list",        signature(x = "Customer"),        as.list.Customer) 

#' @title Convert Objects to data.frames
#' @name as.data.frame  
#' @aliases as.data.frame,Customer-method 
#' @rdname as.data.frame-methods 
setMethod("as.data.frame",  signature(x = "Customer"),        as.data.frame.Customer) 

setAs("data.frame", "Customer", def=function(from){return(as.Customer.data.frame(from))})
setAs("list", "Customer", def=function(from){return(as.Customer.list(from))})
################################### is... - Method ###################################################

#' @title Checks if an Object is as Customer
#' @export
#' @name is.Customer
#' @aliases is.Customer
#' @param x the object to be checked for class \code{\link{Customer}}
#' @param ... optional Parameters (not supported)
setGeneric("is.Customer",       function(x, ...) standardGeneric( "is.Customer")) 

#' @aliases is.Customer,Customer-method 
#' @rdname is.Customer
setMethod( "is.Customer", "Customer", function(x, ...){return(is(x ,"Customer"))})

#' @title How many Customers are included?
#' @name length 
#' @aliases length,Customer-method 
#' @rdname length 
setMethod("length", "Customer",
          function(x){
            N <- length(x@id)
            return(N)
          } 
) 



