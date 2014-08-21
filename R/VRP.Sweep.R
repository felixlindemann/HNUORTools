#' @name VRP.SWEEP 
#' @rdname VRP.SWEEP 
#' @title Vehicle-Routing-Problem -- SWEEP-Algorithm
#'
#' @description Calculate solution for the VRP using the SWEEP-Algorithm.
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{VRP.SWEEP}}}{
#'    \describe{ 
#'      \item{alpha}{ numeric The \code{alpha}-Shape-Parameter of the SWEEP-algorithm (Offset of Sweep-Angle. To be rovided in Radians!)}
#'      \item{constraint}{ numeric \emph{Optional Parameter}. Defining the maximum loading-capacity of each tour. Default is 2*sum(demand) +1 (will be ignored).}
#'      \item{constraint.maxstops}{numeric \emph{Optional Parameter}. Defining the maximum Stops of each tour. Default is 2*n +1 (will be ignored).}
#'      \item{roundcij}{ Obsolete. Please use digits from \code{\link{getDistance}}.}
#'      \item{log}{logical Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{getpolar}} (offset-parameter is used by \code{\link{VRP.SWEEP}}}
#'    }
#' }  
#' @return same modified object of Type \code{\link{GeoSituation}}.
#'      The Solution will be assigned to each \code{\link{Warehouse}$vrp}
#' @keywords OR Vehicle-Routing-Problem VRP SWEEP
#' @details Apart from Algorithm 5.1 at Domschke(2010) p. 229, the improvement steps with 2opt/3opt are not implemented.
#' @export 
#' @references Algorithmus 5.1 S. 229: Domschke, Wolfgang (2010): Logistik. Rundreisen und Touren. 5., Aufl. Muenchen [u.a.]: Oldenbourg (2).
#'
#' @seealso \code{\link{GeoSituation}}, \code{\link{Node}}, \code{\link{VRP.SWEEP}} \code{\link{VRP.SAVINGS}}
#' @examples
#' # demo(HNUVRP01)
#' # demo(HNUVRP02) 
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
setGeneric("VRP.SWEEP",  function(object,...)  standardGeneric("VRP.SWEEP") )

#' @aliases VRP.SWEEP,GeoSituation-method
#' @rdname VRP.SWEEP
setMethod("VRP.SWEEP", signature(object="GeoSituation"),
          function(object,...){ 
            message("VRP.SWEEP\n")
            li <- list(...) 
            
            
            if(is.null(li$log)) li$log <- TRUE
            if(is.null(li$roundcij)) li$roundcij <- TRUE 
            if(is.null(li$rotateClockwise)) li$rotateClockwise <- FALSE
            if(is.null(li$alpha)) li$alpha <- 0
            
            if(is.null(object$tpp$x)) object$tpp$x <- matrix()
            
            
            M <- length(object$warehouses)
            N <- length(object$customers)
            totalcosts <- 0
            
            if( length(object$tpp$x) == 1){
              
              if(M!=1)
                stop("There is no Transportplan assigned. This is required as there are more than 1 warehouses.")
              
              x <- matrix(object$customers$demand, nrow=1, byrow=TRUE)
              
              rownames(x) <- object$warehouses$id
              colnames(x) <- object$customers$id
              
              object$tpp$x <- x
              
            }else{
              
              if(nrow(object$tpp$x)!=M)
                stop("There is no valid Transportplan assigned. The number of warehouses is not equal to the number of rows in the transportation plan.")
              
              if(ncol(object$tpp$x)!=N)
                stop("There is no valid Transportplan assigned. The number of customers is not equal to the number of columns in the transportation plan.")
              
            }
            
            if(!is.null(li$cij)){
              object$tsp$cij <- li$cij
            }
            
            if(length(object$tsp$cij) <= 1){
              
              nodes <- list()
              
              for ( i in 1:M){
                
                nodes[[length(nodes)+1]] <- object$warehouses[i]
                
              }
              
              for ( j in 1:N){
                
                nodes[[length(nodes)+1]] <- object$customers[j]
                
              }
              
              K <- length(nodes)
              
              cij <- matrix(rep(0, K*K), ncol=K, nrow=K, byrow=TRUE)
              
              for (i in 1:(K-1))
              {
                cij[i,i] <-0
                n1 <- nodes[[i]]
                
                for(j in (i+1):K){
                  n2 <- nodes[[j]]
                  cij[i,j] <- getDistance(n1,n2,...)
                  cij[j,i] <- cij[i,j]
                }
                
              } 
              
              object$tsp$nodes <- nodes
              object$tsp$cij <- cij
              if(li$log){
                warning("\nCij has been calculated.\nNodes have been assigned to TSP.\n") 
              }
              
            }else{  
              
              if(nrow(object$tsp$cij)!=M+N)
                stop("There is no valid costmatrix assigned. The number of warehouses is not equal to the number of rows in the transportation plan.")
              
              if(ncol(object$tsp$cij)!=N+M)
                stop("There is no valid costmatrix assigned. The number of customers is not equal to the number of columns in the transportation plan.")
              
            }
            
            
            
            if(is.null(li$constraint.maxstops)) { 
              li$constraint.maxstops <- N*2+1
              if(li$log)  message("Maximum number of stops is not given. Constraint not taken into account.")
            }
            if(is.null(li$constraint.cap)) { 
              li$constraint.cap <- sum(object$tpp$x)*2+1
              if(li$log) message("Maximum Vehicle Loading Capacity is not given. Constraint not taken into account.")
            } 
            if(is.null(li$constraint.dur)) { 
              li$constraint.dur <- sum(cij)*2+1
              if(li$log) message("Maximum Vehicle Tour Duration is not given. Constraint not taken into account.")
            } 
            
            for(i in 1:M){
              # iterate over all warehouses
              w<- object$warehouses[i]
              if(li$log){
                cat("Analyzing warehouse:",w$label,"\n")
              }
              df<-data.frame()
              for(j in 1:N){ 
                if(object$tpp$x[i,j] >0){
                  cust <- object$customers[j] 
                  df<-rbind(df, data.frame(j = j , polar=getpolar(w,cust, offset = li$alpha,  ...) ))
                } 
              } 
              # sort by col2.
              if(li$rotateClockwise ){
                df <- df[ order( -df[,2]), ]
              } else{
                df <- df[ order( df[,2]), ]
              } 
              vrp <- list()
              vrp$customers<-df
              vrp$maxstops <- li$constraint.maxstops 
              vrp$maxcapacity <- li$constraint.cap
              vrp$maxduration <- li$constraint.dur
              
              vrp$x   <- object$tsp$cij * 0
              vrp$cij <- object$tsp$cij
              
              vrp$tours <- list()
              
              tour <- NULL
              
              vi <- i
              for( j in 1:nrow(df)){
                
                vj <- df$j[j] + M
                k <- df$j[j] 
                
                NT <- FALSE 
                if(is.null(tour)){
                  NT <- TRUE 
                } else{ 
                  # checking loading capcity
                  if(tour$loading + object$tpp$x[i,k]   > li$constraint.cap){
                    NT <- TRUE
                    if(li$log) cat("\t\t Maximum loading capacity exceeded, if next customer (",object$customers$label[k],"demand:", object$tpp$x[i,k]  ,") will be added (",tour$loading ,"+",  object$tpp$x[i,k]  ,">", li$constraint.cap,").",
                        "\n\t\t I am Going to end the current Tour.\n")
                  } else{
                    # checking maximum number of stops
                    if(length(tour$stops) >li$constraint.maxstops){
                      NT <- TRUE
                      
                      if(li$log) cat("\t\t Maximum number of stops exceeded, if next customer (",object$customers$label[k],") will be added (",length(tour$stops) ,">",li$constraint.maxstops,").",
                          "\n\t\t I am Going to end the current Tour.\n")
                    }else{
                      # checking maximum number of stops
                      if(tour$costs + object$tsp$cij[vi,vj] +vrp$cij[vj, i]   >li$constraint.dur){
                        NT <- TRUE
                        if(li$log) cat("\t\t Maximum cost/duration exceeded, if next customer (",object$customers$label[k],") will be added (",tour$costs ++ object$tsp$cij[vi,vj] +vrp$cij[vj, i]  ,">",li$constraint.dur,").",
                            "\n\t\t I am Going to end the current Tour.\n")
                      }
                    }
                  } 
                } 
                if(NT){	
                  NT <- FALSE
                  if(!is.null(tour)){
                    # Add Warehouse as last stop
                    vrp$x[vi,i] <- 1
                    tour$costs <- tour$costs +  vrp$cij[vi, i] 
                    # store old Tour
                    vrp$tours[[length(vrp$tours)+1]] <- tour   
                    if(li$log){
                      cat("\t\t Tourcosts:",tour$costs  ,"\n")
                    }  	
                  }
                  if(li$log) cat("\tTour #",length(vrp$tours)+1,":\n") 
                  tour <- list()
                  tour$loading <- 0
                  tour$costs <- 0	
                  tour$stops <-0
                  tour$stops.list    <- list()
                  tour$stops.indices <- numeric()
                  vi <- i
                }	
                
                if(li$log){
                  cat("\t\t",object$customers$label[k], "(demand:",object$tpp$x[i,k] ,", angle:",df$polar[j],")","\n")
                }
                tour$loading <- tour$loading +  object$tpp$x[i,k] 
                tour$stops <- tour$stops + 1
                tour$stops.list[[length(tour$stops.list)+1]] <- object$customers[k]
                tour$stops.indices <- c(tour$stops.indices, vj)
                
                tour$costs <- tour$costs + object$tsp$cij[vi,vj]
                vrp$x[vi,vj]<- 1
                vi <- vj
              }
              if(!is.null(tour)){
                if(tour$stops > 0){
                  # Add Warehouse as last stop
                  vrp$x[tour$stops.indices[length(tour$stops.indices)],i] <- 1
                  tour$costs <- tour$costs + 
                    vrp$cij[
                      tour$stops.indices[length(tour$stops.indices)],
                      i
                      ] 
                  # store old Tour
                  vrp$tours[[length(vrp$tours)+1]] <- tour  	
                  if(li$log){
                    cat("\t\t No customers left. \n\t\t I am Going to end the current Tour.\n")
                    cat("\t\t Tourcosts:",tour$costs  ,"\n")
                  }  					
                  
                }
              } 
              vrp$F <- sum(object$tsp$cij*vrp$x)
              totalcosts <- totalcosts + vrp$F
              if(li$log){
                cat("\tTotalcosts for ",w$label,":",vrp$F ,"\n")
              } 
              object$warehouses$vrp[[i]]  <- vrp
            }
            if(li$log){
              message(paste("Total costs (all Warehouses) are:", totalcosts))
            }
            return(object)
          }       
)
