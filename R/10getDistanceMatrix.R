#' @name getDistanceMatrix
#' @rdname getDistanceMatrix 
#' @title Calculates the distance matrix for a given origin-destination set 
#' @description calculates the euklidian-distance.
#' calls the function \code{\link{getDistance}(...)}
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param sorigin String taking one of the following values Node, Customer, Warehouse
#' @param sdestination String taking one of the following values Node, Customer, Warehouse
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{getDistanceMatrix}}}{
#'    \describe{ 
#'    \item{\code{...}:}{no optional parameters are used by this function directly.} 
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{getDistance}}:}{Parameters are forwarded.} 
#'    }
#' } 
#' @keywords OR Euklid Distance
#' @export   
#' @seealso \code{\link{getDistance}}, code{\link{GeoSituation}}, code{\link{Node}}  
#' @examples
#' #  
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
#'			\url{http://felixlindemann.blogspot.de}
setGeneric("getDistanceMatrix",  function(object,sorigin,sdestination,...)  standardGeneric("getDistanceMatrix") )
#' @aliases getDistanceMatrix,GeoSituation,character,character-method
#' @rdname getDistanceMatrix 
setMethod("getDistanceMatrix",signature(object="GeoSituation",sorigin="character",sdestination="character"),
          function(object,sorigin,sdestination,...){
            li<-list(...) 
            fields<- c( "Node", "Customer", "Warehouse")
            
            f<-agrep(sorigin, fields,  ignore.case = TRUE)
            if(length(f)!=1){stop("The origin-Data-Source could not be identified")}
            t<-agrep(sdestination, fields,  ignore.case = TRUE)
            if(length(t)!=1){stop("The To-Data-source could not be identified")}
            
            
            origin 		<- list()
            destination <- list()
            
            if(f == 1){
              origin<- object@nodes
            }else if(f == 2){
              origin<- object@customers
            }else if(f == 3){
              origin<- object@warehouses
            }else{
              stop(paste("The origin-Data-Source could not be identified. unexpected value: ", f)) 
            }
            if(t == 1){
              destination<- object@nodes
            }else if(t == 2){
              destination<- object@customers
            }else if(t == 3){
              destination<- object@warehouses
            }else{
              stop(paste("The Destination-Data-Source could not be identified. unexpected value: ", f))
            }
            
            if(length(origin) == 0){
              stop("The Origin-Datasource is empty")
            }
            if(length(destination) == 0){
              stop("The destination-Datasource is empty")
            }
            
            I <- length(origin)
            J <- length(destination)
            
            m<-matrix(rep(NA,I*J), nrow =I, ncol=J)
            
            for(i in 1:I){
              n1<- origin[i]
              for(j in 1:J){
                n2<- destination[j]
                m[i,j] <- getDistance(n1,n2, ...) 
              }
            }  
            
            rownames(m) <- origin$id
            colnames(m) <- destination$id
            
            
            
            return(m)
          }
)