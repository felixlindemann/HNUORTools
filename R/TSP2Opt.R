#' @name TSP.2OPT 
#' @rdname TSP.2OPT 
#' @title Travelling-Salesman-Problem -- Two-Opt
#'
#' @description Improves a given Route by switching links.
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TSP.3OPT}}}{
#'    \describe{ 
#'   	\item{log}{\code{"logical"} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}  
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{...}} currently not forewarded.}
#'    }
#' }  
#' @keywords OR Travelling-Salesman-Problem TSP 2-opt two-opt
#' @details Explain what two-opt does.
#' @export  
#' @return same modified object of Type \code{\link{GeoSituation}}.
#'      The Solution will be assigned the attribute \code{tsp}.
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
#'			\url{http://felixlindemann.blogspot.de}
setGeneric("TSP.2OPT",  function(object,...)  standardGeneric("TSP.2OPT") )

#' @aliases TSP.2OPT,GeoSituation-method
#' @rdname TSP.2OPT
setMethod("TSP.2OPT", signature(object="GeoSituation"),
          function(object,...){ 
            message("TSP.2OPT\n")
            li <- list(...) 
            
            if(is.null(li$maxiter))  li$maxiter  <- NA
            if(is.null(li$log))  li$log  <- FALSE 
            if(is.null(li$tsp))  li$tsp <- object$tsp
            if(is.null(li$tsp) | length(li$tsp)==0) stop("No TSP found. Please provide a TSP.")
            
            R   <- li$tsp$roundtrip 
            n   <- length(R)-1
            cij <- li$tsp$cij
            locations <- li$tsp$nodes$label
            
            found<-TRUE
            if(li$log){
              cat("\taktuelle Route:\n")
              print(locations[R])
            }
            iteration <-0
            while(found){
              found <- FALSE 
              iteration <- iteration+1
              for(i in 1:(n-2)){ 
                for(j in (i+2):(n)){ 
                  
                  
                  vi<-c(R[i],R[i+1])
                  vj<-c(R[j],R[j+1])
                  if(vj[2] > n) vj[2] <- R[1]
                  
                  c.alt <- cij[vi[1],vi[2]] +cij[vj[1],vj[2]]
                  c.neu <- cij[vi[1],vj[1]] +cij[vi[2],vj[2]]
                  
                  if(li$log == TRUE) {
                    cat("Pruefe ob ", 
                        locations[vi[1]], "-", 
                        locations[vj[1]]," und ", 
                        locations[vi[2]], "-", 
                        locations[vj[2]]," kuerzer ist als ", 
                        locations[vi[1]], "-", 
                        locations[vi[2]]," und ", 
                        locations[vj[1]], "-", 
                        locations[vj[2]]," =  c.alt:",round(c.alt)," > c.neu: ",round(c.neu)," = ",c.alt>c.neu," \n") 
                  }
                  
                  if(c.alt>c.neu){
                    if(li$log == TRUE) {
                      cat("Tausche ", 
                          locations[vi[1]], "-", 
                          locations[vj[1]]," und ", 
                          locations[vi[2]], "-", 
                          locations[vj[2]]," mit ", 
                          locations[vi[1]], "-", 
                          locations[vi[2]]," und ", 
                          locations[vj[1]], "-", 
                          locations[vj[2]],". Ersparnis: ",round(c.alt - c.neu),"\n") 
                    }
                    
                    
                    
                    #neue Rundreise
                    R <- c(R[1:i], R[j:(i+1)], R[(j+1):(n+1)])
                    li$tsp$roundtrip<-R
                    # Neuer Zielfunktionswert
                    
                    x<-cij*0
                    for(k in 2:length(R)){
                      x[R[k-1],R[k] ]<-1
                    }
                    li$tsp$F<-sum(cij*x) 
                    li$tsp$x<-x
                    li$tsp$roundtrip<-R
                    
                    if(li$log == TRUE){
                      cat("\tNeue Route:\n")
                      print(locations[R])
                      if(length(R)!=n+1){
                        cat("\n\n\n --------------------- ERRROR --------------------- \n---------------Neue Route zu lang --------------- \n\n")
                        cat("i:",i," - j:",j,"\n")
                        print(locations[R])
                      }
                    } 
                    
                    found<-TRUE 
                    break
                  }
                }
                if(found == TRUE){
                  break
                }
              }
              if(!is.na(li$maxiter)){
                if(iteration > li$maxiter){
                  break
                }
              }
            } 
            li$tsp$iteration <- iteration
            return(li$tsp)
          }
)