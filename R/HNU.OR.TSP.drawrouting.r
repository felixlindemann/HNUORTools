#' @name TSP.drawrouting 
#' @rdname TSP.drawrouting 
#' @title Travelling-Salesman-Problem -- drawrouting
#'
#' @description Draw the current solution in an existing plot.
#' @param object Object of Type \code{\link{GeoSituation}} 
#' @param ... \emph{Optional Parameters} See Below.
#'    
#'). 
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by TSP.drawrouting}{
#'    \describe{ 
#'      \item{startnode.bg.col}{\emph{Optional Parameter} Defines the background-color of the start-node. Default value is 2.}
#'      \item{startnode.bg.pch}{\emph{Optional Parameter} Defines the background-point-Style of the start-node. Default value is 20.}
#'      \item{startnode.bg.cex}{\emph{Optional Parameter} Defines the background-size of the start-node. Default value is 3.}
#'      \item{startnode.fg.col}{\emph{Optional Parameter} Defines the foreground-color of the start-node. Default value is 1.}
#'      \item{startnode.fg.pch}{\emph{Optional Parameter} Defines the foreground-point-Style of the start-node. Default value is 20.}
#'      \item{startnode.fg.cex}{\emph{Optional Parameter} Defines the foreground-size of the start-node. Default value is 1.}
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{arrows} Arrwos-Parameters \code{x1} and \code{y1} are used by \code{TSP.drawrouting}.}
#'    }
#' }
#' @keywords OR Travelling-Salesman-Problem TSP 3-opt Three-opt
#' @details Explain what Three-opt does.
#' @export  
#' @references Domschke
#' @seealso \code{\link{GeoSituation}}, \code{\link{Node}}, \code{\link{TSP.NearestNeighbor}}, \code{\link{TSP.2OPT}},  \code{\link{TSP.3OPT}}
#' @examples
#' # demo(HNUTSP01)
#' # demo(HNUTSP02) 
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
setGeneric("TSP.drawrouting",  function(object,...)  standardGeneric("TSP.drawrouting") )

#' @aliases TSP.drawrouting,GeoSituation-method
#' @rdname TSP.drawrouting
 setMethod("TSP.drawrouting", signature(object="GeoSituation"),
  function(object,...){ 
    message("TSP.drawrouting\n")
  	li <- list(...) 

    if(is.null(li$startnode.bg.col)) li$startnode.bg.col <- 2
    if(is.null(li$startnode.bg.pch)) li$startnode.bg.pch <- 20
    if(is.null(li$startnode.bg.cex)) li$startnode.bg.cex <- 3

    if(is.null(li$startnode.fg.col)) li$startnode.fg.col <- 1
    if(is.null(li$startnode.fg.pch)) li$startnode.fg.pch <- 20
    if(is.null(li$startnode.fg.cex)) li$startnode.fg.cex <- 1

  	n<- length(object$tsp.nodes)
  	if(n <=1) stop("This is not a TSP.")
  	if(nrow(object$tsp.solution$x) != n) stop("Error. TSP not correctly initalized")

  	n1 <- object$tsp.nodes[[object$tsp.solution$StartNode ]]
    points(n1$x, n1$y, cex=li$startnode.bg.cex, pch=li$startnode.bg.pch, col=li$startnode.bg.col)
    points(n1$x, n1$y, cex=li$startnode.fg.cex, pch=li$startnode.fg.pch, col=li$startnode.fg.col) 
  	for(i in 1:n){
  		n1<- object$tsp.nodes[[i]]

  		for(j in 1:n){

  			if(object$tsp.solution$x[i,j] == 1){

  				n2<-object$tsp.nodes[[j]]

  				arrows(n1$x, n1$y, x1 = n2$x, y1 = n2$y, ...) 
  			} 
  		} 
  	} 
  }
)
 