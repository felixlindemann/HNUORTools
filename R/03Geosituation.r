#' @exportClass GeoSituation
#' @name GeoSituation 
#' @rdname GeoSituation
#' @aliases GeoSituation-class 
#' @title The GeoSituation class
#' 
#'  @description
#' This class is part of the \pkg{HNUORTools}. It can be understood as a scenario-placeholder in an
#' \dfn{Operations-Research (OR)}-context.
#' 
#' @details Find here the defined slots for this class.
#' @section Slots defined: 
#' \describe{
#'    \item{\code{id}:}{Object of class \code{"character"}, The Id-of the Scenario.} 
#'    \item{\code{label}:}{Object of class \code{"character"}, The label-of the Scenario.} 
#'    \item{\code{nodes}:}{Objects of class \code{\link{Node}} used in the Scenario.} 
#'    \item{\code{links}:}{Objects of class \code{\link{Link}} used in the Scenario.} 
#'    \item{\code{warehouses}:}{Objects of class \code{\link{Warehouse}} used in the Scenario.} 
#'    \item{\code{customers}:}{Objects of class  \code{\link{Customer}} used in the Scenario.} 
#'    \item{\code{spp}:}{Object of class \code{\link{list}}. The items in the list are:
#'      \describe{
#'        \item{\code{iteration}:}{an \code{\link{numeric}} value indicating after which iteration the algorithm terminated.}
#'        \item{\code{Q}:}{A \code{\link{data.frame}} (sorted list) with the reachable nodes.}
#'        \item{\code{tableau}:}{A nx2 \code{\link{matrix}} with the current solution. In Column "p" is the Index(!) of the predecessor (not the id) stored.}
#'        \item{\code{finaltableau}:}{A \code{\link{matrix}} with the results of every iteration. In Column "p" is the Index(!) of the predecessor (not the id) stored.}
#'      }
#'    }
#'    \item{\code{tpp}:}{Object of class \code{\link{list}}. The items in the list are:
#'      \describe{
#'        \item{x}{An object of type \code{\link{matrix}}, representing the Transportation Plan.}
#'        \item{cij}{An object of type \code{\link{matrix}}, representing the Transportation Costs.}
#'        \item{costfactor}{An object of type \code{\link{matrix}}, representing the Transportation Cost factor.}
#'      }
#'    } 
#'  } 
#'       @section Creating objects of type \code{\link{GeoSituation}}:
#'          \describe{
#'              \item{Creating an \code{S4-Object}}{ 
#'                  \code{new("GeoSituation", ...)}
#'              }
#'              \item{Using the user-friendly function}{ 
#'                  \code{new("GeoSituation",...)}
#'              } 
#'          }
#'      @section Methods:
#' \describe{ 
#'    \item{\code{\link{plot}(x=GeoSituation, y=NULL, ...)}}{
#'      Creates a plot the current Situation.
#'    } 
#'  }    
#' @seealso \code{\link{Node}}, \code{\link{Link}}, \code{\link{Warehouse}}, \code{\link{Customer}}
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
#' # \code{citation("HNUORTools"} 
#'
setClass(
  Class = "GeoSituation",
  representation=representation(
    id              = "character",
    label           = "character",
    nodes           = "Node",
    links           = "list",
    warehouses      = "Warehouse",
    customers       = "Customer", 
    spp             = "list", 
    tpp             = "list", 
    tsp             = "list", 
    wlp             = "list"
  ),
  prototype=prototype(
    list(
      id            = character(),
      label         = character(),
      nodes         = NULL,
      warehouses    = NULL,
      links         = list(),
      customers     = NULL,
      spp           = list(),
      tpp           = list(),
      tsp           = list(),
      wlp           = list()
    )
  )
)

################################### initialize - Method ###################################################
#' @title initialize Method
#' @name initialize
#' @aliases initialize,GeoSituation-method 
#' @rdname initialize-methods  
setMethod("initialize", signature="GeoSituation", function(.Object, data=NULL, ...) {
  
  li <- list(...)
  N<-1  
  if(is.null(li$id)) {
    tmp.id <-NULL
    for(i in 1:N){
      tmp.id <- c(tmp.id, UUIDgenerate()) 
    }
    li$id <- tmp.id
    w <- paste("Random ID (",li$id,") provided. Uniqueness should be given.")
    
  }
  if(is.null(li$label)) {
    li$label <- li$id 
  }
  .Object@id     <- as.character(li$id)
  .Object@label  <- as.character(li$label)
  
  .Object@nodes$id[1] <-"dummy"
  .Object@customers$id[1] <-"dummy"
  .Object@warehouses$id[1] <-"dummy"
  #.Object@links$id[1] <-"dummy"
  
  
  validObject(.Object)
  
  return(.Object ) 
})

#####################
#####################
#####################
#####################

#' @title Extract Methods
#' @name $
#' @rdname Extract-methods-1
#' @aliases $,GeoSituation-method  
setMethod("$","GeoSituation",function(x,name) {return(slot(x,name))})

#' @title Extract Methods
#' @name [
#' @aliases [,GeoSituation-method 
#' @rdname Extract-methods-2 
setMethod("[", "GeoSituation",
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
#' @aliases $<-,GeoSituation-method 
#' @rdname Set-methods-1 
setMethod("$<-","GeoSituation",function(x,name,value) {
  slot(x,name,check=TRUE) <- value
  valid<-validObject(x)
  return(x)
})
 
#####################
#####################
#####################



#' @title plot
#' @name plot
#' @aliases plot,GeoSituation-method  
#' @param x the \code{\link{GeoSituation}-Object} to be plotted
#' @param y ommited value for this implementation
#' @param ... list of optional values (see below)
#' @rdname plot-GeoSituation
#' @details Optional Parameters for this Methdod.
#'      @section optional arguments: 
#'        \describe{
#'        
#'              \item{ as used in \code{plot.default}:}{
#'                  The parameters \code{xlim}, \code{ylim}, \code{xlab}, \code{ylab}, 
#'                  \code{main}, \code{sub}
#'                  recieve a default value, if not given. By default \code{asp=1} is set and should not be changed.
#'              }
#'              \item{\code{plotBorders}:}{
#'                   \code{\link{logical}} default = \code{FALSE}. If \code{TRUE}, the borders from the dataset bordersgermany.polygon will be drawn.
#'              }
#'              \item{\code{plotcities}:}{
#'                  \code{\link{logical}} default = \code{FALSE}. If \code{TRUE}, the cities from the dataset bordersgermany.cities will be drawn.
#'              }
#'              \item{\code{plotGrid}:}{
#'                  \code{\link{logical}} default = \code{FALSE}. If \code{TRUE}, a grid will be drawn using the function \code{\link{grid}}.
#'              }
#'              \item{\code{drawNodes}:}{
#'                  \code{\link{logical}} default = \code{TRUE}. If \code{TRUE}, all \code{\link{Node}s} in \code{x (of type \link{GeoSituation}} will be plotted.
#'              }
#'              \item{\code{drawWarehouses}:}{
#'                  \code{\link{logical}} default = \code{TRUE}. If \code{TRUE}, all \code{\link{Warehouse}s} in \code{x (of type \link{GeoSituation}} will be plotted.
#'              }
#'              \item{\code{drawCustomers}:}{
#'                  \code{\link{logical}} default = \code{TRUE}. If \code{TRUE}, all \code{\link{Customer}s} in \code{x (of type \link{GeoSituation}} will be plotted by calling \code{\link{drawCustomers}}.
#'              }
#'              \item{\code{drawLinks}:}{
#'                  \code{\link{logical}} default = \code{TRUE}. If \code{TRUE}, all \code{\link{Link}s} in \code{x (of type \link{GeoSituation}} will be plotted.
#'              } 
#'        } 
#'
setMethod("plot", signature = c("GeoSituation", "ANY"), 
          ## alt. declaration setMethod("plot", signature = "GeoSituation"
          ## missing Arguments will be handled as "ANY" by default
          definition = function(x, y = NULL, ...){ 
            
            li<-list(...)
            if(is.null(li$asp)) li$asp <- 1
            if(is.null(li$xlab)) li$xlab <- ""
            if(is.null(li$ylab)) li$ylab <- ""
            if(is.null(li$main)) li$main <- ""
            
            if(is.null(li$plotGrid))    li$plotGrid<- FALSE 
            if(is.null(li$plotBorders)) li$plotBorders<- FALSE
            if(is.null(li$plotCities))  li$plotCities <- FALSE 
            if(is.null(li$drawVRP))  li$drawVRP <- FALSE 
            if(is.null(li$drawTSP))  li$drawTSP <- FALSE 
            if(is.null(li$drawTPP))  li$drawTPP <- FALSE 
            if(is.null(li$drawWLP))  li$drawWLP <- FALSE 
            
            if(is.null(li$drawNodes)) {
              if(length(x$nodes) ==1 & x$nodes$id[1] == "dummy"){
                li$drawNodes<- FALSE
              }else{
                li$drawNodes<- TRUE
              }
            }
            if(is.null(li$drawLinks)) li$drawLinks<- TRUE
            if(is.null(li$drawCustomers)) {
              if(length(x$customers) ==1 & x$customers$id[1] == "dummy"){
                li$drawCustomers<- FALSE
              }else{
                li$drawCustomers<- TRUE
              }
            }
            if(is.null(li$drawWarehouses)) {
              if(length(x$warehouses) ==1 & x$warehouses$id[1] == "dummy"){
                li$drawWarehouses<- FALSE
              }else{
                li$drawWarehouses<- TRUE
              }
            } 
            
            if(li$plotBorders){
              
              message("plotGeoSitatuon.bordersgermany\n")
              bordersgermany.polygon <- NULL
              cities <- NULL
              
              if(is.null(cities)) data(bordersgermany, envir = environment())
              #stop("Cities of Germany not found. Has the data 'bordersgermany' been loaded?")
              if(is.null(bordersgermany.polygon)) stop("Polygons of Germany not found. Has the data 'bordersgermany' been loaded?")
              
              
              if(is.null(li$xlim)) li$xlim<- c(0,620)
              if(is.null(li$ylim)) li$ylim<- c(0,850) 
              if(is.null(li$polygons.border.color)) li$polygons.border.color<- 1
              if(is.null(li$polygons.fill.color)) li$polygons.fill.color<- "gray98"
              
              
              if(is.null(li$plotPolygons)) li$plotPolygons <- 1:length(bordersgermany.polygon)
              if(is.null(li$plotCities))   li$plotCities <- TRUE
              
              plot(NA, NA, xlim = li$xlim, ylim = li$ylim, asp=li$asp, xlab="", ylab="")
              
              for(i in li$plotPolygons){
                l<-bordersgermany.polygon[[i]]  
                for(j in 1:length(l)){ 
                  l.c <-l[[j]]
                  
                  #lines(l.c[,1], l.c[,2], ...) 
                  polygon(c(l.c[,1],l.c[1,1]), c(l.c[,2],l.c[1,2]), 
                          border = li$polygons.border.color , 
                          col = li$polygons.fill.color )
                } 
              } 
              if(li$plotCities)
                points(cities$x, cities$y, ...)
              
            }else{
              
              if(is.null(li$xlim)){#  
                if(li$drawNodes)      li$xlim <- c(li$xlim, x$nodes$x)
                if(li$drawCustomers)  li$xlim <- c(li$xlim, x$customers$x)
                if(li$drawWarehouses) li$xlim <- c(li$xlim, x$warehouses$x)
                
                li$xlim <- range(li$xlim) + c(-10,10)
              }#
              if(is.null(li$ylim)){#  
                if(li$drawNodes)      li$ylim <- c(li$ylim, x$nodes$y)
                if(li$drawCustomers)  li$ylim <- c(li$ylim, x$customers$y)
                if(li$drawWarehouses) li$ylim <- c(li$ylim, x$warehouses$y) 
                li$ylim <- range(li$ylim)  + c(-10,10)
              }#
              plot(NULL,NULL, #
                   xlim = li$xlim, #
                   ylim = li$ylim,#
                   xlab = li$xlab,#
                   ylab = li$ylab,#
                   main = li$main,#
                   sub  = li$sub,
                   asp  = li$asp
              )#
            }  
            if(li$drawLinks) drawLinks(x, ...) 
            if(li$drawNodes) drawNodes(x, ...) 
            if(li$drawWarehouses) drawWarehouses(x, ...) 
            if(li$drawCustomers) drawCustomers(x, ...)
            if(li$drawVRP) drawVRP(x, ...) 
            if(li$drawTSP) drawTSP(x, ...) 
            if(li$drawTPP) drawTPP(x, ...) 
            if(li$drawWLP) drawWLP(x, ...) 
           
          })


#' @title drawCustomers
#' @name drawCustomers  
#' @aliases drawCustomers
#' @rdname drawCustomers
#' @param object the \code{\link{GeoSituation}-Object(s)} to be plotted 
#' @param ... list of optional values (see below) 
#' @export
#' @details Optional Parameters for this Methdod.
#'      @section optional arguments: 
#'        \describe{ 
#'              \item{\code{zoom}:}{
#'                  \code{\link{numeric}} default = \code{0.1}. Used as a scale for the House-Polygons.
#'              } 
#'              \item{\code{withlabels}:}{
#'                  \code{\link{logical}} default = \code{TRUE}. If \code{TRUE}, labels will be printed for each customer.
#'              } 
#'              \item{\code{customer.font.cex}:}{
#'                  \code{\link{numeric}} default = 1. Defines with which font-size the labels will be printed.                  
#'              } 
#'              \item{\code{customer.font.col}:}{
#'                  \code{\link{numeric} or \link{character}} default = \code{"black"}. The color the labels should be printed in.
#'                  Can be an Array of length of Customers to provide different colors. By default, the provided value will be 
#'                  copied.
#'              } 
#'              \item{\code{customer.bg.col}:}{
#'                  \code{\link{numeric} or \link{character}} default = \code{"gray"}. The color the warehouses-background should be printed in.
#'                  Can be an Array of length of Customers to provide different colors. By default, the provided value will be 
#'                  copied.
#'              } 
#'              \item{\code{customer.borders.col}:}{
#'                  \code{\link{numeric} or \link{character}} default = \code{"black"}. The color the warehouses-outline should be printed in.
#'                  Can be an Array of length of Customers to provide different colors. By default, the provided value will be 
#'                  copied.
#'              }  
#'              \item{\code{customer.point.pch}:}{
#'                  \code{\link{numeric}} default = \code{20}. defines the pch of the customers. can be an array of length of customers. 
#'                  By default, the provided value will be copied.
#'              } 
#'              \item{\code{customer.point.cex}:}{
#'                  \code{\link{numeric}} default = \code{1}. defines the cex of the customers. can be an array of length of customers. 
#'                  By default, the provided value will be copied.
#'              } 
#'        } 
setGeneric("drawCustomers",  function(object,...)  standardGeneric("drawCustomers") )
#' @name drawCustomers
#' @aliases drawCustomers,GeoSituation-method
#' @rdname drawCustomers
setMethod("drawCustomers", signature = c("GeoSituation"), 
          ## alt. declaration setMethod("drawCustomers", signature = "GeoSituation"
          ## missing Arguments will be handled as "ANY" by default
          definition = function(object, ...){  
            li<-list(...)
            n<-length(object$customers)
            if(n>0){
              if(is.null(li$zoom )) li$zoom <- .5
              if(is.null(li$withlabels)) li$withlabels<- TRUE
              
              if(is.null(li$customer.font.cex )) li$customer.font.cex <- 1
              if(is.null(li$customer.font.col )) li$customer.font.col <- "black"
              
              if(is.null(li$customer.bg.col )) li$customer.bg.col <- "gray"      
              if(is.null(li$customer.border.col )) li$customer.border.col <- "black"      
              
              if(is.null(li$customer.point.pch )) li$customer.point.pch <- 20
              if(is.null(li$customer.point.cex )) li$customer.point.cex <- 1#
              
              # Copy, if not of length n
              if(length(li$customer.font.col) != n) li$customer.font.col <- rep(li$customer.font.col, n)
              if(length(li$customer.bg.col) != n)   li$customer.bg.col   <- rep(li$customer.bg.col, n)
              if(length(li$customer.point.pch) != n) li$customer.point.pch <- rep(li$customer.point.pch, n)
              if(length(li$customer.border.col) != n) li$customer.border.col <- rep(li$customer.border.col, n)
              
              if(n == 1 && object$customers$id[1]=="dummy"){
                #don't plot --> its the intial value
              }else{ 
                for(i in 1:n){
                  customer <- object$customers[i]
                  
                  dx<-(c(0,0,-0.5,1,2.5,2,2,0)-1)*li$zoom + customer$x#
                  dy<-(c(0,2,2,3.5,2,2,0,0)-1.5) *li$zoom + customer$y#
                  
                  polygon(dx, dy, col=li$customer.bg.col[i], border = li$customer.border.col[i])#
                  
                  if(li$withlabels){
                    text(max(dx),min(dy)-1.5*li$zoom, customer$id, 
                         cex=li$customer.font.cex[i],
                         col=li$customer.font.col[i])#
                  } 
                }
                points(object$customers$x,object$customers$y,
                       pch=li$customer.point.pch,
                       cex=li$customer.point.cex
                )#
              }
              #message("hier2")
              li$withlabels <-li$withlabels
            }
          }
)


#' @title drawWarehouses
#' @name drawWarehouses  
#' @rdname drawWarehouses
#' @param object the \code{\link{GeoSituation}-Object(s)} to be plotted 
#' @param ... list of optional values (see below) 
#' @export
#' @details Optional Parameters for this Methdod.
#'      @section optional arguments: 
#'        \describe{ 
#'              \item{\code{zoom}:}{
#'                  \code{\link{numeric}} default = \code{0.1}. Used as a scale for the House-Polygons.
#'              } 
#'              \item{\code{withlabels}:}{
#'                  \code{\link{logical}} default = \code{TRUE}. If \code{TRUE}, labels will be printed for each warehouse.
#'              } 
#'              \item{\code{isWLP}:}{
#'                  \code{\link{logical}} default = \code{FALSE}. If \code{TRUE}, closed Warehouses will be printed blank.
#'              } 
#'              \item{\code{warehouse.font.cex}:}{
#'                  \code{\link{numeric}} default = 1. Defines with which font-size the labels will be printed.                  
#'              } 
#'              \item{\code{warehouse.font.col}:}{
#'                  \code{\link{numeric} or \link{character}} default = \code{"black"}. The color the labels should be printed in.
#'                  Can be an Array of length of Warehouses to provide different colors. By default, the provided value will be 
#'                  copied.
#'              } 
#'              \item{\code{warehouse.bg.col}:}{
#'                  \code{\link{numeric} or \link{character}} default = \code{"gray"}. The color the warehouses-background should be printed in.
#'                  Can be an Array of length of Warehouses to provide different colors. By default, the provided value will be 
#'                  copied.
#'              } 
#'              \item{\code{warehouse.borders.col}:}{
#'                  \code{\link{numeric} or \link{character}} default = \code{"black"}. The color the warehouses-outline should be printed in.
#'                  Can be an Array of length of Warehouses to provide different colors. By default, the provided value will be 
#'                  copied.
#'              }  
#'              \item{\code{warehouse.point.pch}:}{
#'                  \code{\link{numeric}} default = \code{20}. defines the pch of the warehouses. can be an array of length of warehouses. 
#'                  By default, the provided value will be copied.
#'              } 
#'              \item{\code{warehouse.point.cex}:}{
#'                  \code{\link{numeric}} default = \code{1}. defines the cex of the warehouses. can be an array of length of warehouses. 
#'                  By default, the provided value will be copied.
#'              } 
#'        } 
setGeneric("drawWarehouses",  function(object,...)  standardGeneric("drawWarehouses") )
#' @name drawWarehouses
#' @aliases drawWarehouses,GeoSituation-method
#' @rdname drawWarehouses
setMethod("drawWarehouses", signature = c("GeoSituation"), 
          ## alt. declaration setMethod("drawWarehouses", signature = "GeoSituation"
          ## missing Arguments will be handled as "ANY" by default
          definition = function(object, ...){  
            li<-list(...)
            n<-length(object$warehouses)
            if(n>0){
              if(is.null(li$zoom )) li$zoom <- .5
              if(is.null(li$withlabels)) li$withlabels<- TRUE
              if(is.null(li$isWLP)) li$isWLP<- FALSE
              
              if(is.null(li$warehouse.font.cex )) li$warehouse.font.cex <- 1
              if(is.null(li$warehouse.font.col )) li$warehouse.font.col <- "black"
              
              if(is.null(li$warehouse.bg.col )) li$warehouse.bg.col <- "gray"      
              if(is.null(li$warehouse.border.col )) li$warehouse.border.col <- "black"      
              
              if(is.null(li$warehouse.point.pch )) li$warehouse.point.pch <- 20
              if(is.null(li$warehouse.point.cex )) li$warehouse.point.cex <- 1#
              
              # Copy, if not of length n
              if(length(li$warehouse.font.col) != n) li$warehouse.font.col <- rep(li$warehouse.font.col, n)
              if(length(li$warehouse.bg.col) != n)   li$warehouse.bg.col   <- rep(li$warehouse.bg.col, n)
              if(length(li$warehouse.point.pch) != n) li$warehouse.point.pch <- rep(li$warehouse.point.pch, n)
              if(length(li$warehouse.border.col) != n) li$warehouse.border.col <- rep(li$warehouse.border.col, n)
              
              for(i in 1:n){
                warehouse <- object$warehouses[i]
                
                if(li$isWLP & warehouse$open == 0) li$warehouse.bg.col <- "white"
                
                dx<-(c(0,0,1,1,2,2,3,3,4,4,5,5,0)-2.5)*li$zoom + warehouse$x#
                dy<-(c(0,3.5,2,3.5,2,3.5,2,3.5,2,5,5,0,0)-1.5) *li$zoom + warehouse$y#
                
                polygon(dx, dy, col=li$warehouse.bg.col[i], border = li$warehouse.border.col[i])#
               
                if(li$withlabels){
                  text(max(dx),min(dy)-1.5*li$zoom, warehouse$id, 
                       cex=li$warehouse.font.cex[i],
                       col=li$warehouse.font.col[i])#
                } 
              }
              points(object$warehouses$x,object$warehouses$y,
                     pch=li$warehouse.point.pch,
                     cex=li$warehouse.point.cex 
              )#
            }
          }
)


#' @title drawNodes
#' @name drawNodes  
#' @rdname drawNodes
#' @export
#' @param object the \code{\link{GeoSituation}-Object(s)} to be plotted 
#' @param ... list of optional values (see below) 
#' @export
#' @details Optional Parameters for this Methdod.
#'      @section optional arguments: 
#'        \describe{  
#'              \item{\code{withlabels}:}{
#'                  \code{\link{logical}} default = \code{TRUE}. If \code{TRUE}, labels will be printed for each Node.
#'              }  
#'              \item{\code{pch}:}{
#'                  \code{\link{numeric}} default = 21. Defines the point-Type to be used.
#'              } 
#'              \item{\code{point.cex}:}{
#'                  \code{\link{numeric}} default = 3. Defines the size of the points.
#'              } 
#'              \item{\code{font.cex}:}{
#'                  \code{\link{numeric}} default = 3. Defines the size of the font.
#'              }  
#'              \item{\code{bg}:}{
#'                  \code{\link{numeric} or \link{character}} default = \code{"white"}. The background-color the node-background should be printed in.
#'              } 
#'              \item{\code{col}:}{
#'                  \code{\link{numeric} or \link{character}} default = \code{"black"}. The foreground-color the node-outline should be printed in.
#'              }  
#'        } 
setGeneric("drawNodes",  function(object,...)  standardGeneric("drawNodes") )

#' @name drawNodes
#' @aliases drawNodes,GeoSituation-method
#' @rdname drawNodes
setMethod("drawNodes",signature(object="GeoSituation"),
  function(object,...){
    li<-list(...)
    n<-length(object$nodes)
    if(n>0){
      
      if(is.null(li$withlabels)) li$withlabels<- TRUE
      
      if(is.null(li$pch)) li$pch<- 21
      if(is.null(li$point.cex)) li$point.cex<- 3
      if(is.null(li$font.cex)) li$font.cex<- 0.75
      
      if(is.null(li$bg))  li$bg<- "white"
      if(is.null(li$col)) li$col<- 1
      
      x <- object$nodes$x#
      y <- object$nodes$y#
      points(x,y,pch = li$pch, bg = li$bg , cex=li$point.cex)#
      
      if(li$withlabels){
        text(x,y, object$nodes$id, cex=li$font.cex,col=li$col)#
      }
    }
  }
)


#' @title drawLinks
#' @name drawLinks  
#' @rdname drawLinks
#' @export
#' @param object the \code{\link{GeoSituation}-Object(s)} to be plotted 
#' @param ... list of optional values (see below) 
#' @export
#' @details Optional Parameters for this Methdod.
#'      @section optional arguments: 
#'        \describe{  
#'              \item{\code{withlabels}:}{
#'                  \code{\link{logical}} default = \code{TRUE}. If \code{TRUE}, labels will be printed for each Node.
#'              }  
#'              \item{\code{pch}:}{
#'                  \code{\link{numeric}} default = 21. Defines the point-Type to be used.
#'              } 
#'              \item{\code{point.cex}:}{
#'                  \code{\link{numeric}} default = 3. Defines the size of the points.
#'              } 
#'              \item{\code{font.cex}:}{
#'                  \code{\link{numeric}} default = 3. Defines the size of the font.
#'              }  
#'              \item{\code{bg}:}{
#'                  \code{\link{numeric} or \link{character}} default = \code{"white"}. The background-color the node-background should be printed in.
#'              } 
#'              \item{\code{col}:}{
#'                  \code{\link{numeric} or \link{character}} default = \code{"black"}. The foreground-color the node-outline should be printed in.
#'              }  
#'        } 
setGeneric("drawLinks",  function(object,...)  standardGeneric("drawLinks") )

#' @name drawLinks
#' @aliases drawLinks,GeoSituation-method
#' @rdname drawLinks
setMethod("drawLinks",signature(object="GeoSituation"),
  function(object,...){
    li<-list(...)
    n<-length(object$links)
    if(n>0){
      
      if(is.null(li$pch)) li$pch<- 21
      if(is.null(li$p.cex)) li$lines.p.cex<- 2
      if(is.null(li$t.cex)) li$lines.t.cex<- 0.5
      if(is.null(li$lwd)) li$lwd<- 1
      if(is.null(li$bg))  li$bg<- "white" 
      if(is.null(li$lines.markused)) li$lines.markused<- TRUE
      if(is.null(li$lines.plotlength)) li$lines.plotlength<- FALSE
      if(is.null(li$lines.plotcosts)) li$lines.plotcosts<- FALSE
      if(is.null(li$lwd.used)) li$lwd.used<- li$lwd*3
      if(is.null(li$lty)) li$lty<- 1 
      if(is.null(li$col)) li$col<- 1 
      if(is.null(li$colused)) li$colused<- "red"
      
      for(i in 1:n){
        link <- object$links[[i]]
        x<- c(link$origin$x, link$destination$x)
        y<- c(link$origin$y, link$destination$y)
        if(link$used & li$lines.markused){ 
          lines(x,y,lty=li$lty, lwd=li$lwd.used, col=li$colused)#
        }
        lines(x,y,lty=li$lty, lwd=li$lwd, col=li$col)#
        if(li$lines.plotlength){
          x <- sum(x)/2
          y <- sum(y)/2
          points(x,y,pch = li$pch, bg = li$bg , cex=li$lines.p.cex)# 
          if(li$lines.plotcosts){
            text(x,y, link$costs, cex=li$lines.t.cex,col=li$col)#
          }else{
            text(x,y, link$distance, cex=li$lines.t.cex,col=li$col)#
          }
        }
      }
    }
  } 
)

#' @title drawVRP
#' @name drawVRP  
#' @rdname drawVRP-method
#' @export
#' @param object the \code{\link{GeoSituation}-Object(s)} to be plotted 
#' @param ... list of optional values (see below) 
#' @export
#' @details Optional Parameters for this Methdod.
#'      @section optional arguments: 
#'        \describe{  
#'              \item{\code{drawroutingtowarehouse}:}{
#'                  \code{\link{logical}} default = \code{TRUE}. If \code{TRUE}, routes will be connected to the delivering \code{\link{Warehouse}}.
#'              }    
#'              \item{\code{arrow.bg.col}:}{
#'                  \code{\link{numeric}} or \code{\link{character}} default = 1. A vector of length 1 or M (=number of \code{\link{Warehouse}s}). Colornames are accepted as well
#'              }    
#'        } 
#'      @section arguments are forwarded to the following functions: 
#'        \describe{  
#'              \item{\code{arrows}:}{ the routing will be drawn using this functions. attributes \code{x1},\code{y1},\code{col} are blocked }    
#'        } 
setGeneric("drawVRP",  function(object,...)  standardGeneric("drawVRP") )

#' @aliases drawVRP,GeoSituation
#' @rdname drawVRP-method
setMethod("drawVRP",signature(object="GeoSituation"),
          function(object, ...){
           # message("drawVRP\n")
            li <- list(...)   
            #check if is VRP
            M<- length(object$warehouses)
            if(M < 1) stop("This is not a VRP - no warehouse has been found.")
            # set default values
            if(is.null(li$drawroutingtowarehouse)) li$drawroutingtowarehouse <- TRUE
            
            if(is.null(li$log ))     li$log   		<- FALSE
            if(is.null(li$arrow.Tips ))     li$arrow.Tips   		<- .15
            if(is.null(li$arrow.Code )) 		li$arrow.Code 			<- 2
            if(is.null(li$arrow.lwd ))  		li$arrow.lwd 			<- 2 
            if(is.null(li$arrow.bg.col ))    	li$arrow.bg.col 		<- 1
            if(length(li$arrow.bg.col)!=M) li$arrow.bg.col <- rep(li$arrow.bg.col, M)
            
            #iterate over all warehouses (multiwarehouse vrp.)
            for(m in 1:M){
              #get Current Warehouse
              w <- object$warehouses[m] 
              
              vrp<- object$warehouses$vrp[[m]]
              # if tours exists 
              if(!is.null(vrp$tours)){
                #iterate over all tours
                for(t in 1:length(vrp$tours)){ 
                  #select current tour
                  tour <- vrp$tours[[t]]
                  
                  if(length(tour)>1){
                    #get the number of stops
                    n<-tour$stops
                    
                    if (n==0){
                      # do nothing - if no stops are assigned. 
                    }else if(n ==1){    
                      # if it's a "pendeltour", draw both ways
                      # code = 3 doesn't work for generality reasons,
                      # if code=3 is assigned as default, 
                      # users will not be able to define the arrows-code-attribute
                      # without getting errors.
                      n1<- tour$stops.list[[1]]    
                      arrows(
                        w$x, w$y, x1 = n1$x, y1 = n1$y,  
                        code  = li$arrow.Code,
                        length	= li$arrow.Tips,
                        lwd		= li$arrow.lwd,
                        col		= li$arrow.bg.col[m]
                      )  
                      arrows(
                        n1$x, n1$y, x1 = w$x, y1 = w$y,
                        code  = li$arrow.Code,
                        length	= li$arrow.Tips,
                        lwd		= li$arrow.lwd,
                        col		= li$arrow.bg.col[m]
                      )  
                    } else{
                      # more than one stop is assigned
                      for(i in 1:(n-1)){
                        # get first stop
                        n1<- tour$stops.list[[i]] 
                        
                        if(li$drawroutingtowarehouse & i==1){ 
                          # draw connection of depot, 
                          # if this variable is set
                          # draw the connection from the depot to the first stop
                          arrows(
                            w$x, w$y, x1 = n1$x, y1 = n1$y,
                            code  = li$arrow.Code,
                            length  = li$arrow.Tips,
                            lwd		= li$arrow.lwd,
                            col		= li$arrow.bg.col[m]
                          )   
                          # draw the connection from the last stop to depot
                          n2 <- tour$stops.list[[n]]
                          arrows(
                            n2$x, n2$y, x1 = w$x, y1 = w$y,
                            code  = li$arrow.Code,
                            length  = li$arrow.Tips,
                            lwd  	= li$arrow.lwd,
                            col		= li$arrow.bg.col[m]
                          )    
                        }  
                        # get following node of n1 and draw the connection
                        n2<-tour$stops.list[[i+1]] 
                        arrows(
                          n1$x, n1$y, x1 = n2$x, y1 = n2$y,
                          code  = li$arrow.Code,
                          length  = li$arrow.Tips,
                          lwd    = li$arrow.lwd,
                          col		= li$arrow.bg.col[m]
                        )                                 
                      }  
                    }
                  }
                }
              }  
            } 
          }
)

#' @title drawTSP
#' @name drawTSP  
#' @rdname drawTSP-method
#' @export
#' @param object the \code{\link{GeoSituation}-Object(s)} to be plotted 
#' @param ... list of optional values (see below) 
#' @export
#' @details Optional Parameters for this Methdod.
#'      @section optional arguments: 
#'        \describe{  
#'            \item{startnode.bg.col}{\emph{Optional Parameter} Defines the background-color of the start-node. Default value is 2.}
#'            \item{startnode.bg.pch}{\emph{Optional Parameter} Defines the background-point-Style of the start-node. Default value is 20.}
#'            \item{startnode.bg.cex}{\emph{Optional Parameter} Defines the background-size of the start-node. Default value is 3.}
#'            \item{startnode.fg.col}{\emph{Optional Parameter} Defines the foreground-color of the start-node. Default value is 1.}
#'            \item{startnode.fg.pch}{\emph{Optional Parameter} Defines the foreground-point-Style of the start-node. Default value is 20.}
#'            \item{startnode.fg.cex}{\emph{Optional Parameter} Defines the foreground-size of the start-node. Default value is 1.}
#'        } 
#'      @section arguments are forwarded to the following functions: 
#'        \describe{  
#'              \item{\code{arrows}:}{ the routing will be drawn using this functions. attributes \code{x1},\code{y1}  are blocked }    
#'        } 
setGeneric("drawTSP",  function(object,...)  standardGeneric("drawTSP") )

#' @aliases drawTSP,GeoSituation
#' @rdname drawTSP-method
setMethod("drawTSP",signature(object="GeoSituation"),
          function(object, ...){
            message("TSP.drawrouting\n")
            li <- list(...) 
            
            if(is.null(li$arrow.Tips ))   	li$arrow.Tips 			<- .15
            if(is.null(li$arrow.Code )) 		li$arrow.Code 			<- 2
            if(is.null(li$arrow.lwd ))  		li$arrow.lwd 			<- 2 
            if(is.null(li$arrow.bg.col ))    	li$arrow.bg.col 		<- 1
            
            if(is.null(li$startnode.bg.col)) li$startnode.bg.col <- 2
            if(is.null(li$startnode.bg.pch)) li$startnode.bg.pch <- 20
            if(is.null(li$startnode.bg.cex)) li$startnode.bg.cex <- 3
            
            if(is.null(li$startnode.fg.col)) li$startnode.fg.col <- 1
            if(is.null(li$startnode.fg.pch)) li$startnode.fg.pch <- 20
            if(is.null(li$startnode.fg.cex)) li$startnode.fg.cex <- 1
            
            n<- length(object$tsp$nodes)
            if(n <=1) stop("This is not a TSP.")
            if(nrow(object$tsp$x) != n) stop("Error. TSP not correctly initalized")
            
            n1 <- object$tsp$nodes[object$tsp$StartNode ]
            points(n1$x, n1$y, cex=li$startnode.bg.cex, pch=li$startnode.bg.pch, col=li$startnode.bg.col)
            points(n1$x, n1$y, cex=li$startnode.fg.cex, pch=li$startnode.fg.pch, col=li$startnode.fg.col) 
            for(i in 1:n){
              n1<- object$tsp$nodes[i] 
              for(j in 1:n){ 
                if(object$tsp$x[i,j] == 1){ 
                  n2<-object$tsp$nodes[j]  
                  arrows(
                    n1$x, n1$y, x1 = n2$x, y1 = n2$y,
                    code	= li$arrow.Code,
                    length	= li$arrow.Tips,
                    lwd		= li$arrow.lwd,
                    col		= li$arrow.bg.col
                  ) 
                } 
              } 
            } 
          }
)

#' @title drawWLP
#' @name drawWLP  
#' @rdname drawWLP
#' @export
#' @param object the \code{\link{GeoSituation}-Object(s)} to be plotted 
#' @param ... list of optional values (see below) 
#' @export
#' @details Optional Parameters for this Methdod.
#'      @section optional arguments: 
#'        \describe{  
#'              \item{\code{...}:}{
#'                  Is forwarded to \code{\link{drawTPP}}.
#'              }   
#'        } 
setGeneric("drawWLP",  function(object,...)  standardGeneric("drawWLP") )

#' @name drawWLP
#' @aliases drawWLP,GeoSituation-method
#' @rdname drawWLP
setMethod("drawWLP",signature(object="GeoSituation"),
  function(object,...){
    drawTPP(object, isWLP=TRUE, ...)
  } 
)

#' @title drawTPP
#' @name drawTPP  
#' @rdname drawTPP
#' @export
#' @param object the \code{\link{GeoSituation}-Object(s)} to be plotted 
#' @param ... list of optional values (see below) 
#' @export
#' @details Optional Parameters for this Methdod.
#'      @section optional arguments: 
#'        \describe{  
#'              \item{\code{withlabels}:}{
#'                  \code{\link{logical}} default = \code{TRUE}. If \code{TRUE}, labels will be printed for each Node.
#'              }   
#'        } 
setGeneric("drawTPP",  function(object,...)  standardGeneric("drawTPP") )

#' @name drawTPP
#' @aliases drawTPP,GeoSituation-method
#' @rdname drawTPP
setMethod("drawTPP",signature(object="GeoSituation"),
  function(object,...){
    li<-list(...) 
    x<- NA
    if(is.null(li$isWLP)) li$isWLP <- FALSE
    
    if(li$isWLP){
      x <- object$wlp$x		# store transportplan locally
    }else{
      x <- object$tpp$x		# store transportplan locally
    }
    I <- length(object$warehouses)	
    J <- length(object$customers) 
    if(length(x) == I * J){
      
      if(is.null(li$arrow.textposition )) li$arrow.textposition 	<- 2/3
      if(is.null(li$arrow.Tips )) 		li$arrow.Tips 			<- .15
      if(is.null(li$arrow.Code )) 		li$arrow.Code 			<- 2
      if(is.null(li$arrow.lwd ))  		li$arrow.lwd 			<- 2 
      if(is.null(li$arrow.bg.col ))  		li$arrow.bg.col 		<- 1
      if(is.null(li$arrow.font.col )) 	li$arrow.font.col 		<- 1
      if(is.null(li$arrow.font.cex )) 	li$arrow.font.cex 		<- .75
      if(is.null(li$arrow.point.col )) 	li$arrow.point.col 		<- 1
      if(is.null(li$arrow.point.pch )) 	li$arrow.point.pch 		<- 21
      if(is.null(li$arrow.point.bg )) 	li$arrow.point.bg 		<- "white"
      if(is.null(li$arrow.point.cex )) 	li$arrow.point.cex 		<- 4 
      if(is.null(li$arrow.cex )) 			li$arrow.cex 			<- 1 
      
      if(length(li$arrow.bg.col	)!=I) 	li$arrow.bg.col 		<- rep(li$arrow.bg.col, I)
      if(length(li$arrow.font.col )!=I) 	li$arrow.font.col 		<- rep(li$arrow.font.col, I)
      if(length(li$arrow.point.bg )!=I) 	li$arrow.point.bg 		<- rep(li$arrow.point.bg, I)
      
      for(i in 1:I){
        warehouse <- object$warehouses[i]
        for(j in 1:J){
          customer <- object$customers[j]
          if(x[i,j]> 0) { 
            arrows(
              warehouse$x,warehouse$y, 	# From 
              customer$x,customer$y, 		# TO
              code	= li$arrow.Code,
              length	= li$arrow.Tips,
              lwd		= li$arrow.lwd,
              col		= li$arrow.bg.col[i]
            ) 
            if(!li$isWLP){
              
              points( warehouse$x + li$arrow.textposition *(customer$x-warehouse$x),
                      warehouse$y + li$arrow.textposition *(customer$y-warehouse$y), 
                      pch=li$arrow.point.pch,
                      cex=li$arrow.point.cex,
                      bg =li$arrow.point.bg[i],
                      col=li$arrow.point.col
              ) 
              text(warehouse$x + li$arrow.textposition *(customer$x-warehouse$x),
                   warehouse$y + li$arrow.textposition *(customer$y-warehouse$y), 
                   x[i,j], 
                   cex=li$arrow.font.cex,
                   col=li$arrow.font.col
              ) 
              
            }
          }
        } 
      }
    } 
  } 
)
##############################################################
#' @name add 
#' @docType methods
#' @export  
#' @rdname GeoSituation-Add-method
#'
#' @title Add an Object to a \code{\link{GeoSituation}}
#' @description This method should simplify the use of HNUORTools.
#'  
#' @param object an object of class \code{\link{GeoSituation}}
#' @param value an object of class \code{\link{Node}}, \code{\link{Link}}, \code{\link{Customer}} or \code{\link{Warehouse}}
#' @param ... Additional argument list that might not ever be used.
#' @return The updated object of class \code{\link{GeoSituation}}.
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
#'      \url{http://felixlindemann.blogspot.de}
setGeneric("add",  function(object,value,...)  standardGeneric("add") )

#' @aliases add,GeoSituation,Node-method
#' @rdname GeoSituation-Add-method
setMethod("add",signature(object="GeoSituation", value="Node"),
          function(object,value,...){
            if(validObject(value) & is.Node(value)){
              n<- length(object@nodes)
              if(n==1 & object@nodes$id[1] == "dummy"){
                object@nodes<- value
              }else{
                
                df<-as.data.frame(object@nodes)
                df<-rbind(df,as.data.frame(value))
                object@nodes <- as.Node(df)
              } 
            } 
            return(object)
          }
)


#' @aliases add,GeoSituation,Customer-method
#' @rdname GeoSituation-Add-method
setMethod("add",signature(object="GeoSituation", value="Customer"),
          function(object,value,...){
            if(validObject(value) & is.Customer(value)){
              n<- length(object@customers)
              if(n==1 & object@customers$id[1] == "dummy"){
                object@customers<- value
              }else{
                
                df<-as.data.frame(object@customers)
                df<-rbind(df,as.data.frame(value))
                object@customers <- as.Customer(df)
              } 
            } 
            return(object)
          }
)

#' @aliases add,GeoSituation,Warehouse-method
#' @rdname GeoSituation-Add-method
setMethod("add",signature(object="GeoSituation", value="Warehouse"),
          function(object,value,...){
            if(validObject(value) & is.Warehouse(value)){
              n<- length(object@warehouses)
              if(n==1 & object@warehouses$id[1] == "dummy"){
                
                object@warehouses$id <- value$id 
                object@warehouses$label <- value@label 
                object@warehouses$x     <- value@x 
                object@warehouses$y     <- value@y
                object@warehouses$supply     <- value@supply
                object@warehouses$fixcosts     <- value@fixcosts
                object@warehouses$open     <- value@open
                object@warehouses$isDummy     <- value@isDummy
                object@warehouses$vrp     <- value@vrp
                  
              }else{
                id <- c( object@warehouses$id , value$id )
                label <- c( object@warehouses$label ,  value@label )
                x <- c( object@warehouses$x ,  value@x )
                y <- c( object@warehouses$y ,  value@y)
                supply <- c( object@warehouses$supply ,  value@supply)
                fixcosts <- c( object@warehouses$fixcosts ,  value@fixcosts)
                open <- c( object@warehouses$open ,  value@open)
                isDummy <- c( object@warehouses$isDummy ,  value@isDummy) 
                
                vrp <- object@warehouses@vrp
                for(i in 1:length(value@vrp)){
                  n<-length(vrp)  
                  vrp[[n+1]] <- value@vrp[[i]]
                }
                object@warehouses <- new("Warehouse", id=id, 
                         label = label, 
                         x = x, 
                         y = y, 
                         supply = supply, 
                         fixcosts = fixcosts, 
                         open = open, 
                         isDummy = isDummy)
                object@warehouses@vrp <- vrp 
              }  
            } 
            return(object)
          }
)

#' @aliases add,GeoSituation,Link-method
#' @rdname GeoSituation-Add-method
setMethod("add",signature(object="GeoSituation", value="Link"),
          function(object,value,...){
            if(validObject(value) & is.Link(value)){
              n<- length(object@links)
              
                for(l in object$links){
                  
                  if(l$origin$id == value$origin$id & 
                        l$destination$id == value$destination$id){
                    # same link found
                    warning(paste("The link with id (",value$id,") was not added. The nodes n1(id=",
                                  value$origin$id,") and n2(id=",value$destination$id,
                                  ") are already connected. Please modify the existing link (id=",l$id,") if required."))
                    return(object)
                  } #end 1. if
                  
                  if(l$origin$id == value$destination$id & 
                        l$destination$id == value$origin$id
                        ){
                    if(l$oneway == FALSE){
                      # same link found
                      warning(paste("The link with id (",value$id,") was not added. The nodes n1(id=",
                                    value$origin$id,") and n2(id=",value$destination$id,
                                    ") are already connected. Please modify the existing link (id=",l$id,") if required."))
                      return(object)
                    }else{
                      #existing is marked as oneway
                      if (value$oneway == FALSE){
                        
                        # same link found
                        warning(paste("The link with id (",value$id,") was not added. The nodes n1(id=",
                                      value$origin$id,") and n2(id=",value$destination$id,
                                      ") are already connected by a link that is marked as one-way.",
                                      "Adding this link would violate the existing setup.",
                                      "Please modify the link if (id=",l$id,") required or mark the new link as oneway, too."))
                        return(object)
                      } 
                    } #end else
                    
                  }# end 2nd if
                  
                } # end for
              object$links[[n+1]] <- value
               
            } 
            return(object)
          }
)


          