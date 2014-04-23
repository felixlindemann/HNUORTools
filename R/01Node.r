#' @exportClass Node
#' @name Node 
#' @rdname Node
#' @aliases Node-class 
#' @title The Node class
#'
#' @description 
#' This class is part of the \pkg{HNUORTools}. It represents 
#' the base class for every locateable class in an
#' \dfn{Operations-Research (OR)}-context.
#' 
#' @details Find here the defined slots for this class.
#'      @section Slots: 
#'          \describe{
#'              \item{\code{id}:}{
#'                  Object of class \code{"character"}, containing data from id.
#'                  \strong{Should be unique}.
#'                  The default value will be caluclated randomly.
#'              }
#'              \item{\code{label}:}{
#'                  Object of class \code{"character"}, containing the label of 
#'                  the \code{\link{Node}}.
#'                  The default value will be caluclated randomly.
#'              }
#'              \item{\code{x}:}{
#'                  Object of class \code{"numeric"}, containing the x-coordinate 
#'                  of the \code{\link{Node}}.
#'                  The default value will be caluclated randomly.
#'              }
#'              \item{\code{y}:}{
#'                  Object of class \code{"numeric"}, containing the y-coordinate 
#'                  of the \code{\link{Node}}.
#'                  The default value will be caluclated randomly.
#'              }
#'          }
#'
#' @seealso The classes are derived from this class and the following Methods can be used with this class.:
#'      @section Creating objects of type \code{\link{Node}}:
#'          \describe{
#'              \item{Creating an \code{S4-Object}}{ 
#'                  \code{new("Node", ...)}
#'              } 
#'              \item{Converting from a \code{data.frame}}{ 
#'                  \code{as.Node{<data.frame>}}
#'                  See also below in the Methods-Section.
#'              }
#'              \item{Converting from a \code{list}}{ 
#'                  \code{as.Node{<list>}}
#'                  See also below in the Methods-Section.
#'              }
#'          }
#'      @section Methods:
#'          \describe{
#'              \item{\code{as.list(node, ...)}}{
#'                  Converts a \code{\link{Node}} into a \code{\link{list}}.
#'                  \code{...} are user-defined (not used) parameters.
#'              }
#'              \item{\code{as.data.frame(node, ...)}}{
#'                  Converts a \code{\link{Node}} into a \code{\link{data.frame}}.
#'                  \code{as.data.frame} accepts the optional parameter \code{withrownames} of class \code{"logical"} (default is \code{TRUE}). 
#'                  If \code{withrownames == TRUE} the returned \code{\link{data.frame}} will recieve the \code{id} as rowname.
#'                  \code{...} are user-defined (not used) parameters.
#'
#'              }
#'              \item{\code{as.Node(obj)}}{
#'                  Converts an object of class \code{\link{data.frame}} or of class \code{\link{list}} into a \code{\link{Node}}.
#'              } 
#'              \item{\code{is.Node(obj)}}{
#'                  Checks if the object \code{obj} is of type \code{\link{Node}}.
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
#'          \describe{
#'              \item{\code{\link{Customer}}}{
#'                  This class extends the \code{\link{Node}}-Class 
#'                  with attributes for according to customers for OR-Problems.
#'              }
#'              \item{\code{\link{Warehouse}}}{
#'                  This class extends the \code{\link{Node}}-Class 
#'                  with attributes for according to warehouses for OR-Problems.
#'              }
#'              \item{\code{\link{Link}}}{
#'                  This class is constructed by two entities of the \code{\link{Node}}-Class 
#'                  representing the origin and destination of \code{\link{Link}} for OR-Problems.
#'              }
#'          }
#'      @section To be used for:
#'          \describe{ 
#'              \item{SPP}{
#'                   Shortest-Path-Problem
#'              }
#'              \item{TSP}{
#'                  Travelling Salesman Problem
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
#' # create a new Node with specific values
#' x<- new("Node", x= 10, y=20, id="myid", label = "mylabel")
#' x
#' # create from data.frame
#' df<- data.frame(x=10,y=20)
#' new("Node", df)
#' as(df, "Node")
#' as.Node(df)
#' #create some nodes
#' n1 <- new("Node",x=10,y=20, id ="n1")
#' n2 <- new("Node",x=13,y=24, id ="n1")
#' # calculate Beeline distance
#' #getDistance(n1,n2) # should result 5
#' #getDistance(n1,n2, costs = 2) # should result 10
setClass(
	Class = "Node",
    representation=representation(
    	id     = "character",
        label  = "character",
    	x      = "numeric",
    	y      = "numeric"
    ), 
    validity = function(object){
        
        N <- length(object@id) 
        if(length(object@label) != N) 
            return("Invalid Object of Type 'Node': the length of the attributes 'id' and 'label' differ!")
        if(length(object@x) != N)     
            return("Invalid Object of Type 'Node': the length of the attributes 'id' and 'x' differ!")
        if(length(object@y) != N)     
            return("Invalid Object of Type 'Node': the length of the attributes 'id' and 'y' differ!") 

        if( sum(is.null(object@x)) + sum( is.na(object@x)) > 0 ) 
            return(paste("Invalid Object of Type 'Node': Error with value x: Value is not initialized", class(object@x)))
        
        if( sum(is.null(object@y)) + sum( is.na(object@y)) > 0) 
            return(paste("Invalid Object of Type 'Node': Error with value y: Value is not initialized", class(object@y)))
        
        if( class(object@x)!="numeric" ) 
            return(paste("Invalid Object of Type 'Node': Error with value x: expected numeric datatype, but obtained", class(object@x)))
        
        if( class(object@y)!="numeric" ) 
            return(paste("Invalid Object of Type 'Node': Error with value y: expected numeric datatype, but obtained", class(object@y))) 

        return(TRUE) 
    } 
)
################################### initialize - Method ###################################################
#' @title initialize Method
#' @name initialize
#' @aliases initialize,Node-method 
#' @rdname initialize-methods 
#' @param data can of type \code{\link{data.frame}} or \code{\link{list}}
setMethod("initialize", signature="Node", function(.Object, data=NULL, ...) {
      
    li <- list(...)
    N<-1
    if(is.null(li$showwarnings))  li$showwarnings <- FALSE
    if(!is.null(data)){ 
      
        if(class(data) =="list") {
          data <- as.data.frame(data)
        }
        if(class(data) =="data.frame") {
            N<-nrow(data)
            li$id    <- data$id
            li$label <- data$label 
            li$x     <- data$x
            li$y     <- data$y

        } else{ 
            stop("Error: argument data should be of type 'data.frame'!")
        }
    } 
    if(is.null(li$x)) {
        li$x <- as.numeric(runif(N,0,100)) 
        w <- paste("x-Coordinate simulated (x= ",li$x,").")
        if(li$showwarnings) warning(w) 
    }
    if(is.null(li$y)) {
        li$y <- as.numeric(runif(N,0,100)) 
        w <- paste("y-Coordinate simulated (y= ",li$y,").")
        if(li$showwarnings) warning(w) 
    }
    if(is.null(li$id)) {
        tmp.id <-NULL
        for(i in 1:N){
            tmp.id <- c(tmp.id, UUIDgenerate()) 
        }
        li$id <- tmp.id
        w <- paste("Random ID (",li$id,") provided. Uniqueness should be given.")
        if(li$showwarnings) warning(w) 
    }
    if(is.null(li$label)) {
        li$label <- li$id 
    }
    .Object@id     <- as.character(li$id)
    .Object@label  <- as.character(li$label)

    .Object@x      <- as.numeric(  li$x)
    .Object@y      <- as.numeric(  li$y)
    
    validObject(.Object)
     
    return(.Object ) 
})
################################### extract - Method ###################################################

#' @title Extract Methods
#' @name $
#' @aliases $,Node-method 
#' @rdname Extract-methods-1
#' @param name Name of Attribute.
setMethod("$","Node",function(x,name) {return(slot(x,name))})
#' @title Extract Methods
#' @name [
#' @aliases [,Node-method 
#' @rdname Extract-methods-2
#' @param i Row Index
#' @param j Name or Column index
#' @param drop Optional value for Drop-Levels.
setMethod("[", "Node",
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
        return (new("Node", df))         
    } 
) 
################################### Set - Method ###################################################
 
#' @title Set Methods
#' @name $<- 
#' @aliases $<-,Node-method 
#' @rdname Set-methods-1
#' @param name Name of parameter to change
#' @param value new Value.
setMethod("$<-","Node",function(x,name,value) {
  slot(x,name,check=TRUE) <- value
  valid<-validObject(x)
  return(x)
})

#' @title Set Methods
#' @name [<-
#' @aliases [<-,Node-method 
#' @rdname Set-methods-2
#' @param x The Node
#' @param i row-Index
#' @param j Name or Column index
setReplaceMethod("[",signature(x="Node"),
    function(x,i,j,value){
        N <- length(x)
        if(min(i) <=0) stop("Index i out of bound. It must be positive non zero.")
        if(max(i) >N)  stop("Index i out of bound. It must not be larger than the total length.")
        if(length(value)!=length(i)) stop("The replacement length is not equal to the number of elements to be replaced.")    
         
        if(is(value, "Node")){
                
            # copy original values to tmp vectors
            tmp <- as.data.frame(x)
            

            #replace the item(s) regarding to i
            tmp$id[i]    <- value@id 
            tmp$label[i] <- value@label 
            tmp$x[i]     <- value@x 
            tmp$y[i]     <- value@y
            
            # return new item
            return (new("Node", tmp)) 
        } else {
            if(missing(j)) {
                # call the function recursivly
                # might throw an error if the conversion is not supported.
                x[i] <- as.Node(value)
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
as.Node.list       = function(x, ...){return(new("Node", data=x))}
as.Node.data.frame = function(x, ...){return(new("Node", data=x))}
#local convert message
as.data.frame.Node = function(x, ...){
    li<-list(...)
    if(is.null(li$withrownames)) li$withrownames <- FALSE
    df<-data.frame(id=x@id, label = x@label, x= x@x, y= x@y)
    if(li$withrownames) rownames(df)<-x@id
    return (df)
}
as.list.Node = function(x, ...){list(id=x@id, label = x@label, x= x@x, y= x@y)}


#' @export
setGeneric("as.Node",       function(x, ...) standardGeneric( "as.Node")) 

#' @title Convert Objects to Nodes
#' @name as.Node  
#' @param x an  Object to convert.
#' @param ... optional parameters
#' @aliases as.Node,list-method 
#' @rdname as.Node-methods 
setMethod("as.Node",     signature(x = "list"),       as.Node.list) 

#' @title Convert Objects to Nodes
#' @name as.Node
#' @aliases as.Node,data.frame-method 
#' @rdname as.Node-methods 
setMethod("as.Node",    signature(x = "data.frame"),  as.Node.data.frame)  

#' @title Convert Objects to lists
#' @name as.list
#' @param x the Node  
#' @aliases as.list,Node-method 
#' @rdname as.list-methods 
setMethod("as.list",        signature(x = "Node"),        as.list.Node) 

#' @title Convert Objects to data.frames
#' @name as.data.frame
#' @param x the Node  
#' @aliases as.data.frame,Node-method 
#' @rdname as.data.frame-methods 
setMethod("as.data.frame",  signature(x = "Node"),        as.data.frame.Node) 
 
setAs("data.frame", "Node", def=function(from){return(as.Node.data.frame(from))})
setAs("list", "Node", def=function(from){return(as.Node.list(from))})
################################### is... - Method ###################################################

#' @title Checks if an Object is as Node
#' @export
#' @name is.Node
#' @param x the Node   
#' @param ... optional Parameters (not supported)
setGeneric("is.Node",       function(x, ...) standardGeneric( "is.Node")) 

#' @aliases is.Node,Node-method 
#' @rdname is.Node
setMethod( "is.Node", "Node", function(x, ...){ 
    return(is(x ,"Node")) 
})
 
 
#' @title How many Nodes are included?
#' @name length
#' @param x the Nodes
#' @aliases length,Node-method 
#' @rdname length 
setMethod("length", "Node",
    function(x){
        N <- length(x@id)
        return(N)
    } 
)
 
#' @aliases getDistance,Node,Node-method 
#' @rdname getDistance    
#' @examples 
#' # w1 <- new("Node", x=0, y=0)
#' # c1 <- new("Node",  x=3, y=4)
#' # getDistance(w1,c1) # should result 5.  
#' # getDistance(w1,c1,costfactor=3) # should result 15.  
setMethod( "getDistance", signature = c("Node", "Node"),
    function(n0,n1,   ...) {
        li<-list(...)
        
        p0<-c(n0$x,n0$y)
        p1<-c(n1$x,n1$y)
        
        value <- getDistance(p0,p1, ...)
        
        return(value)
    }
)
  
#' @aliases getpolar,Node,Node-method 
#' @rdname getpolar      
#' @examples
#' # w1 <- new("Node", x=0, y=0)
#' # c1 <- new("Node",  x=1, y=1)
#' # getpolar(w1,c1,deg=TRUE) # should result 45.  
setMethod("getpolar", signature=c("Node", "Node"),
    function(n0,n1,   ...) {
        li<-list(...) 
        validObject(n0)
        validObject(n1)
        
        p0<-c(n0$x,n0$y)
        p1<-c(n1$x,n1$y)

        value <- getpolar(p0,p1, ...)
        
        return(value)
    }
)
 