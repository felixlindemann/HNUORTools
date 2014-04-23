#' @exportClass Warehouse
#' @name Warehouse 
#' @rdname Warehouse
#' @aliases Warehouse-class 
#' @title The Warehouse class
#'
#' @description 
#' This class is part of the \pkg{HNUORTools}. It represents 
#' the base class for every locateable class in an
#' \dfn{Operations-Research (OR)}-context.
#' 
#' @details Find here the defined slots for this class.
#'      @section Slots: 
#'        \describe{
#'              \item{\code{supply}:}{
#'                  Object of class \code{"numeric"}, containing the amount for the supply.
#'                  The default value cannot be negative.
#'              }
#'              \item{\code{fixcosts}:}{
#'                  Object of class \code{"numeric"}, containing the amount for the fixcosts
#'                  The default value cannot be negative.
#'              }
#'              \item{\code{open}:}{
#'                  Object of class \code{"logical"}, indicating if a Warehouse is used within a WLP.
#'              }
#'              \item{\code{vrp}:}{
#'                  Object of class \code{"list"}, a List of Tours genereated for a VRP.
#'              }
#'              \item{\code{isDummy}:}{
#'                  Object of class \code{"logical"}, indicating if a Warehouse was added 
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
#'                  the \code{\link{Warehouse}}.
#'                  The default value will be caluclated randomly.
#'              }
#'              \item{\code{x}:}{
#'                  Object of class \code{"numeric"}, containing the x-coordinate 
#'                  of the \code{\link{Warehouse}}.
#'                  The default value will be caluclated randomly.
#'              }
#'              \item{\code{y}:}{
#'                  Object of class \code{"numeric"}, containing the y-coordinate 
#'                  of the \code{\link{Warehouse}}.
#'                  The default value will be caluclated randomly.
#'              }
#'          }
#'
#' @seealso The classes are derived from this class and the following Methods can be used with this class.:
#'      @section Creating objects of type \code{\link{Warehouse}}:
#'          \describe{
#'              \item{Creating an \code{S4-Object}}{ 
#'                  \code{new("Warehouse", ...)}
#'              } 
#'              \item{Converting from a \code{\link{data.frame}}}{ 
#'                  \code{as.Warehouse{<data.frame>}}
#'                  See also below in the Methods-Section.
#'              }
#'              \item{Converting from a \code{\link{list}}}{ 
#'                  \code{as.Warehouse{<list>}}
#'                  See also below in the Methods-Section.
#'              }
#'          }
#'      @section Methods:
#'          \describe{
#'              \item{\code{as.list(Warehouse, ...)}}{
#'                  Converts a \code{\link{Warehouse}} into a \code{\link{list}}.
#'                  \code{...} are user-defined (not used) parameters.
#'              }
#'              \item{\code{as.data.frame(Warehouse, ...)}}{
#'                  Converts a \code{\link{Warehouse}} into a \code{\link{data.frame}}.
#'                  \code{as.data.frame} accepts the optional parameter \code{withrownames} of class \code{"logical"} (default is \code{TRUE}). 
#'                  If \code{withrownames == TRUE} the returned \code{\link{data.frame}} will recieve the \code{id} as rowname.
#'                  \code{...} are user-defined (not used) parameters. 
#'              }
#'              \item{\code{as.Warehouse(obj)}}{
#'                  Converts an object of class \code{\link{data.frame}} or of class \code{\link{list}} into a \code{\link{Warehouse}}.
#'              } 
#'              \item{\code{is.Warehouse(obj)}}{
#'                  Checks if the object \code{obj} is of type \code{\link{Warehouse}}.
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
#' # create a new Warehouse with specific values
#' x<- new("Warehouse", x= 10, y=20, id="myid", label = "mylabel", supply = 20)
#' x
#' # create from data.frame
#' df<- data.frame(x=10,y=20, supply = 30)
#' new("Warehouse", df)
#' as(df, "Warehouse")
#' as.Warehouse(df)
#' #create some Warehouses
#' n1 <- new("Warehouse",x=10,y=20, supply = 30, id ="n1")
#' n2 <- new("Warehouse",x=13,y=24, supply = 40, id ="n2")
#' # calculate Beeline distance
#' #getDistance(n1,n2) # should result 5
#' #getDistance(n1,n2, costs = 2) # should result 10
setClass(
  Class = "Warehouse",
  representation=representation(
      supply="numeric",
      fixcosts="numeric",
      open = "logical",
      isDummy = "logical",
      vrp="list"
    ), 
  prototype=prototype(
    list(
      id            = character(),
      label         = character(),
      vrp           = list()
    )),
  contains = "Node",
  validity = function(object){
    
    
    df <- as.data.frame(object)
    node <- as.Node(df)
    if(validObject(node)){ 
      N <- length(object@id) 
      if(length(object@supply) != N) 
        return("Invalid Object of Type 'Warehouse': the length of the attributes 'id' and 'supply' differ!")
      if(length(object@fixcosts) != N) 
        return("Invalid Object of Type 'Warehouse': the length of the attributes 'id' and 'fixcosts' differ!")
      if(length(object@open) != N)     
        return("Invalid Object of Type 'Warehouse': the length of the attributes 'id' and 'open' differ!") 
      if(length(object@isDummy) != N)     
        return("Invalid Object of Type 'Warehouse': the length of the attributes 'id' and 'isDummy' differ!")
      
      if( sum(is.null(object@supply)) + sum( is.na(object@supply)) > 0 ) 
        return(paste("Invalid Object of Type 'Warehouse': Error with value supply: Value is not initialized", class(object@supply)))
      
      if( sum(is.null(object@fixcosts)) + sum( is.na(object@fixcosts)) > 0 ) 
        return(paste("Invalid Object of Type 'Warehouse': Error with value fixcosts: Value is not initialized", class(object@fixcosts)))
      
      if( sum(is.null(object@open)) + sum( is.na(object@open)) > 0) 
        return(paste("Invalid Object of Type 'Warehouse': Error with value open: Value is not initialized", class(object@open)))
      
      if( sum(is.null(object@isDummy)) + sum( is.na(object@isDummy)) > 0) 
        return(paste("Invalid Object of Type 'Warehouse': Error with value isDummy: Value is not initialized", class(object@isDummy)))
      
      if( class(object@supply)!="numeric" ) 
        return(paste("Invalid Object of Type 'Warehouse': Error with value supply: expected numeric datatype, but obtained", class(object@supply)))
      
      if( class(object@fixcosts)!="numeric" ) 
        return(paste("Invalid Object of Type 'Warehouse': Error with value fixcosts: expected numeric datatype, but obtained", class(object@fixcosts)))
      
      if( class(object@open)!="logical" ) 
        return(paste("Invalid Object of Type 'Warehouse': Error with value isDummy: expected logical datatype, but obtained", class(object@open))) 
      
      if( class(object@isDummy)!="logical" ) 
        return(paste("Invalid Object of Type 'Warehouse': Error with value isDummy: expected logical datatype, but obtained", class(object@isDummy))) 
      
      if(min(object@supply)<0)
        return(paste("Invalid Object of Type 'Warehouse': Error with value supply: at least one value is negative. Only positive Values are allowed."))
      
      if(min(object@fixcosts)<0)
        return(paste("Invalid Object of Type 'Warehouse': Error with value fixcosts: at least one value is negative. Only positive Values are allowed."))
      
      return(TRUE) 
      
    }else{
      return (FALSE)
    }
  } 
)
################################### initialize - Method ###################################################
#' @title initialize Method
#' @name initialize
#' @aliases initialize,Warehouse-method 
#' @rdname initialize-methods  
setMethod("initialize", signature="Warehouse", function(.Object, data=NULL, ...) {
  
  li <- list(...)
  if(is.null(li$showwarnings))  li$showwarnings <- FALSE
  
  #init from parent Node-Object.
  N<-1
  
  if(!is.null(data)){ 
    
    if(class(data) =="list") {
      data <- as.data.frame(data)
    }
    if(class(data) =="data.frame") {
      N<-nrow(data)
      li$isDummy  <- data$isDummy
      li$open  <- data$open
      li$fixcosts   <- data$fixcosts
      li$supply   <- data$supply
      
    } else{ 
      stop("Error: argument data should be of type 'data.frame'!")
    }
  }  
  if(is.null(li$supply) | length(li$supply) ==0) {
    li$supply <- as.numeric(sample(30:100,N,replace=TRUE)) 
    w <- paste("supply-Coordinate simulated (supply= ",li$supply,").")
    if(li$showwarnings) warning(w) 
  } 
  if(is.null(li$fixcosts) | length(li$fixcosts) ==0) {
    li$fixcosts <- as.numeric(sample(30:100,N,replace=TRUE)) 
    w <- paste("fixcosts-Coordinate simulated (fixcosts= ",li$fixcosts,").")
    if(li$showwarnings) warning(w) 
  } 
  if(is.null(li$open)) {
    li$open <- rep(FALSE,N)
  }
  if(is.null(li$isDummy)) {
    li$isDummy <- rep(FALSE,N)
  }
  
  .Object@supply  <- as.numeric(  li$supply)
  .Object@fixcosts  <- as.numeric(  li$fixcosts)
  .Object@open <- as.logical(  li$open) 
  .Object@isDummy <- as.logical(  li$isDummy) 
  .Object@vrp <- list()
  for(i in 1:N){
    .Object@vrp[[i]] <- list()
  }
  
  
  .Object<-callNextMethod(.Object,data,...) 
  validObject(.Object)
  
  return(.Object ) 
})
################################### extract - Method ###################################################
#' @title Extract Methods
#' @name [
#' @aliases [,Warehouse-method 
#' @rdname Extract-methods-2 
setMethod("[", "Warehouse",
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
            w<- new("Warehouse", df)
            for(k in 1:length(i)){              
              w$vrp[[k]] <- x$vrp[[i[k]]]
            }
            return (w)         
          } 
) 
################################### Set - Method ###################################################

#' @title Set Methods
#' @name [<-
#' @aliases [<-,Warehouse-method 
#' @rdname Set-methods-2 
#' @param i row-Index
#' @param j Name or Column index
setReplaceMethod("[",signature(x="Warehouse"),
                 function(x,i,j,value){
                   N <- length(x)
                   if(min(i) <=0) stop("Index i out of bound. It must be positive non zero.")
                   if(max(i) >N)  stop("Index i out of bound. It must not be larger than the total length.")
                   if(length(value)!=length(i)) stop("The replacement length is not equal to the number of elements to be replaced.")    
                   
                   if(is(value, "Warehouse")){
                     
                     # copy original values to tmp vectors
                     tmp<- as.data.frame(x)
                     
                     #replace the item(s) regarding to i
                     x$id[i]    <- value@id 
                     x$label[i] <- value@label 
                     x$x[i]     <- value@x 
                     x$y[i]     <- value@y
                     x$supply[i]     <- value@supply
                     x$fixcosts[i]     <- value@fixcosts
                     x$open[i]     <- value@open
                     x$isDummy[i]     <- value@isDummy
                     x$vrp[[i]]     <- value@vrp
                      
                     # return new item
                     return (x) 
                   } else {
                     if(missing(j)) {
                       # call the function recursivly
                       # might throw an error if the conversion is not supported.
                       x[i] <- as.Warehouse(value)
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
as.Warehouse.list       = function(x, ...){return(new("Warehouse", data=x))}
as.Warehouse.data.frame = function(x, ...){return(new("Warehouse", data=x))}
#local convert message
as.data.frame.Warehouse = function(x, ...){
  li<-list(...)
  if(is.null(li$withrownames)) li$withrownames <- FALSE
  df<-data.frame(id=x@id, label = x@label, x= x@x, y= x@y, supply=x@supply, fixcosts=x@fixcosts, open = x@open, isDummy = x@isDummy)
  if(li$withrownames) rownames(df)<-x@id
  return (df)
}
as.list.Warehouse = function(x, ...){list(id=x@id, label = x@label, x= x@x, y= x@y, supply=x@supply, fixcosts=x@fixcosts, open = x@open, isDummy = x@isDummy)}


#' @export
setGeneric("as.Warehouse",       function(x, ...) standardGeneric( "as.Warehouse")) 

#' @title Convert Objects to Warehouses
#' @name as.Warehouse  
#' @param x an  Object to convert.
#' @param ... optional parameters
#' @aliases as.Warehouse,list-method 
#' @rdname as.Warehouse-methods 
setMethod("as.Warehouse",     signature(x = "list"),       as.Warehouse.list) 

#' @title Convert Objects to Warehouses
#' @name as.Warehouse
#' @aliases as.Warehouse,data.frame-method 
#' @rdname as.Warehouse-methods 
setMethod("as.Warehouse",    signature(x = "data.frame"),  as.Warehouse.data.frame)  

#' @title Convert Objects to lists
#' @name as.list 
#' @aliases as.list,Warehouse-method 
#' @rdname as.list-methods 
setMethod("as.list",        signature(x = "Warehouse"),        as.list.Warehouse) 

#' @title Convert Objects to data.frames
#' @name as.data.frame 
#' @aliases as.data.frame,Warehouse-method 
#' @rdname as.data.frame-methods 
setMethod("as.data.frame",  signature(x = "Warehouse"),        as.data.frame.Warehouse) 

setAs("data.frame", "Warehouse", def=function(from){return(as.Warehouse.data.frame(from))})
setAs("list", "Warehouse", def=function(from){return(as.Warehouse.list(from))})
################################### is... - Method ###################################################

#' @title Checks if an Object is as Warehouse
#' @name is.Warehouse
#' @export
#' @param x the Warehouse   
#' @param ... optional Parameters (not supported)
setGeneric("is.Warehouse",       function(x, ...) standardGeneric( "is.Warehouse")) 

#' @aliases is.Warehouse,Warehouse-method 
#' @rdname is.Warehouse
setMethod( "is.Warehouse", "Warehouse", function(x, ...){return(is(x ,"Warehouse"))})

#' @title How many Warehouses are included?
#' @name length 
#' @aliases length,Warehouse-method 
#' @rdname length 
setMethod("length", "Warehouse",
          function(x){
            N <- length(x@id)
            return(N)
          } 
)
 