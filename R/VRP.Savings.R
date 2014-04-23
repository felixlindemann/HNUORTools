#' @name VRP.SAVINGS 
#' @rdname VRP.SAVINGS 
#' @title Vehicle-Routing-Problem -- SAVINGS-Algorithm
#'
#' @description Calculate solution for the VRP using the SAVINGS-Algorithm.
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{VRP.SAVINGS}}}{
#'    \describe{ 
#'      \item{alpha}{ numeric The \code{alpha}-Shape-Parameter of the SAVINGS-algorithm (Offset of SAVINGS-Angle. To be rovided in Radians!)}
#'      \item{constraint}{ numeric \emph{Optional Parameter}. Defining the maximum loading-capacity of each tour. Default is 2*sum(demand) +1 (will be ignored).}
#'      \item{constraint.maxstops}{numeric \emph{Optional Parameter}. Defining the maximum Stops of each tour. Default is 2*n +1 (will be ignored).}
#'      \item{roundcij}{ Obsolete. Please use digits from \code{\link{getDistance}}.}
#'      \item{log}{logical Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{getpolar}} (offset-parameter is used by \code{\link{VRP.SAVINGS}}}
#'    }
#' }  
#' @return same modified object of Type \code{\link{GeoSituation}}.
#'      The Solution will be assigned to each \code{\link{Warehouse}$vrp}
#' @keywords OR Vehicle-Routing-Problem VRP SAVINGS
#' @details todo
#' @export 
#' @references Algorithmus 5.3 S. 236: Domschke, Wolfgang (2010): Logistik. Rundreisen und Touren. 5., Aufl. Muenchen [u.a.]: Oldenbourg (2).
#'
#' @seealso \code{\link{GeoSituation}}, \code{\link{Node}}, \code{\link{VRP.SAVINGS}} \code{\link{VRP.SAVINGS}}
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
setGeneric("VRP.SAVINGS",  function(object,...)  standardGeneric("VRP.SAVINGS") )

#' @aliases VRP.SAVINGS,GeoSituation-method
#' @rdname VRP.SAVINGS
setMethod("VRP.SAVINGS", signature(object="GeoSituation"),
          function(object,...){ 
            message("VRP.SAVINGS\n")
            li <- list(...) 
            
            
            if(is.null(li$log)) li$log <- TRUE
            if(is.null(li$roundcij)) li$roundcij <- TRUE  
            if(is.null(li$alpha)) li$alpha <- 1
            if(is.null(li$deltaX)) li$deltaX <- NA
            if(is.null(li$deltaY)) li$deltaY <- NA
            
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
                message("\nCij has been calculated.\nNodes have been assigned to TSP.\n") 
              }
              
            }else{  
              
              if(nrow(object$tsp$cij)!=M)
                stop("There is no valid costmatrix assigned. The number of warehouses is not equal to the number of rows in the transportation plan.")
              
              if(ncol(object$tsp$cij)!=N)
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
            totalcosts <- 0
            for(i in 1:M){
              # iterate over all warehouses
              w<- object$warehouses[i]
              vi<-i
              if(li$log){
                msg<-paste("Analyzing warehouse:",w$label)
                message(msg)
              }
              vrp<-list()
              vrp$tours<- list()
              vrp$F <- 0
              vrp$x <- cij*0
              vrp$cij <- cij
              #create Pendeltouren
              onTour <- data.frame()
              for(j in 1:N){ 
                cust <- object$customers[j]
                vj <- j + M 
                if(object$tpp$x[i,j] >0){
                  tour<- list() 
                  tour$loading <- cust$demand
                  tour$costs <- cij[vi,vj] + cij[vj,vi]  
                  vrp$F <-vrp$F +tour$costs
                  vrp$x[vi,vj]<-1
                  vrp$x[vj,vi]<-1
                  tour$warehouse <- w
                  tour$warehouse.index <- vi
                  tour$stops <- 1
                  tour$stops.list    <- list(cust) 
                  tour$stops.indices <- c(vj)
                  vrp$tours[[length(vrp$tours)+1]] <- tour  
                  onTour <- rbind(onTour,  data.frame(j=j, tour = j))
                }
              } 
              #calculate Savings
              savings <- data.frame()
              for(h in 1:(N-1)){ 
                for(k in (h+1):N){  
                  calcsavings <- TRUE
                  if(!is.na(li$deltaX) & is.numeric(li$deltaX)){
                    if(li$deltaX < abs(object$customers$x[h]-object$customers$x[k])){
                      calcsavings <- FALSE
                    }
                  }
                  if(!is.na(li$deltaY) & is.numeric(li$deltaY)){
                    if(li$deltaY < abs(object$customers$y[h]-object$customers$y[k])){
                      calcsavings <- FALSE
                    }
                  }
                  if(calcsavings){
                    savings <- rbind(savings, data.frame(i =h, 
                                                         vi = h+M, 
                                                         j=k, 
                                                         vj = k+M, 
                                                         sav =  cij[h+M,i] + cij[i,k+M] - li$alpha *  cij[h+M,k+M]  , 
                                                         netto =  cij[h+M,i] + cij[i,k+M] - cij[h+M,k+M], 
                                                         used = FALSE, 
                                                         checked = FALSE  ))
                  }
                }
              }
              
              savings<- savings[order(-savings[,"sav"]),]
            #  message("Savings:\n")
            #  print(savings)
              for(s in 1:nrow(savings)){ 
                h <- savings[s,"i"]
                k <- savings[s,"j"]
                if(li$log) cat("Checking Savings ",object$customers$label[h],"/",object$customers$label[k]," (",savings[s,"sav"],").\n")
                savings[s,"checked"] <- TRUE
                if(savings[s,"sav"]>0){
                  th <- vrp$tours[[onTour[h,"tour"]]] 
                  tk <- vrp$tours[[onTour[k,"tour"]]]
                   
                  if(onTour[h,"tour"]!=onTour[k,"tour"]){
                    if((th$stops.indices[1] == h+M) | (th$stops.indices[th$stops] == h+M)){
                      if((tk$stops.indices[1] == k+M) | (tk$stops.indices[tk$stops] == k+M)){
                        if(th$loading + tk$loading <= li$constraint.cap){
                          if(th$stops + tk$stops <= li$constraint.maxstops){
                            if(th$costs + tk$costs - savings[s,"netto"]<= li$constraint.dur){
                              tour <- list()
                              t1 <- list()
                              t2 <- list()
                              vi <- h+M
                              vj <- k+M
                              if(th$stops.indices[1] == vi ){
                                # th will be attached   xx -- th
                                t2<-th
                                t1<-tk 
                                if(tk$stops.indices[1] == vj){# kt -- th
                                  #tk must be reversed
                                  t1$stops.list    <- rev(t1$stops.list)
                                  t1$stops.indices <- rev(t1$stops.indices) 
                                }   
                              }else{
                                # tk will be attached  th -- xx
                                t1<-th
                                t2<-tk  
                                if(tk$stops.indices[1] != vj){ # th -- kt
                                  #tk mus be reversed
                                  t2$stops.list    <- rev(t2$stops.list)
                                  t2$stops.indices <- rev(t2$stops.indices) 
                                }  
                              }
                              tour<- list() 
                              tour$loading <- t1$loading + t2$loading
                              tour$costs <- t1$costs + t2$costs -savings[s,"netto"]
                              tour$warehouse <- w
                              tour$warehouse.index <- vi
                              tour$stops <-  t1$stops + t2$stops
                              tour$stops.list    <- c( t1$stops.list,  t2$stops.list)
                              tour$stops.indices <-  c( t1$stops.indices,  t2$stops.indices)
                              n<- length(vrp$tours)+1
                              #store new Tour
                              vrp$tours[[n]] <- tour  
                              #update Tourenplan
                              vrp$tours[[onTour[h,"tour"]]] <- list()
                              vrp$tours[[onTour[k,"tour"]]] <- list()
                              
                              #update Customersinfo
                              for(l in tour$stops.indices){
                                onTour[l-M,"tour"] <-n
                              } 
                              
                              #update xij # Reset Tour 1
                              vi<- i
                              vj<-0
                              for(t in th$stops.indices){
                                vj<- t
                                vrp$x[vi,vj] <- 0
                                vi<-vj
                              }
                              vj<-i
                              vrp$x[vi,vj] <- 0
                              
                              #update xij # Reset Tour 2
                              vi<- i
                              vj<-0
                              for(t in tk$stops.indices){
                                vj<- t
                                vrp$x[vi,vj] <- 0
                                vi<-vj
                              }
                              vj<-i
                              vrp$x[vi,vj] <- 0
                              
                              #update xij # set new Tour 
                              vi<- i
                              vj<-0
                              for(t in tour$stops.indices){
                                vj<- t
                                vrp$x[vi,vj] <- 1
                                vi<-vj
                              }
                              vj<-i
                              vrp$x[vi,vj] <- 1
                              
                              #update Targetvalue
                              vrp$F <-vrp$F - savings[s,"netto"]
                              if(li$log) cat("\tnew Tour added.\n")
                              savings[s,"used"]<-TRUE
                            }else{
                              # max duration exceeded
                              if(li$log) cat("\tSavings ignored: Duration/Cost-Capacity would be exceed (",th$costs + tk$costs ,">",li$constraint.dur,").\n")
                            }
                          }else{
                            # max Stops exceeded
                            if(li$log) cat("\tSavings ignored: Maximum-Stops would be exceed (",th$stops + tk$stops ,">",li$constraint.maxstops,").\n")
                          }
                        }else{
                          #Loading capacity exceeded
                          if(li$log) cat("\tSavings ignored: Loading-Capacity would be exceed (",th$loading + tk$loading ,">",li$constraint.cap,").\n")
                        }
                      }else{
                        # k is no end customer
                        if(li$log) cat("\tSavings ignored: Customer '",object$customers$label[k],"' is not an End-Customer.\n")
                      }
                    }else{
                      # h is no end customer
                      if(li$log) cat("\tSavings ignored: Customer '",object$customers$label[h],"' is not an End-Customer.\n")
                    }
                  }else{
                    #customers are on same tour
                    if(li$log) cat("\tSavings ignored: Customers are on the same tour.\n")
                  }
                }else{
                  # check only positive Savings
                  if(li$log) cat("\tnegative Savings ignored.\n")
                } 
              } # end for (Savings)
              #all savings have been checked
              vrp$savings <- savings 
              vrp$tourenplan <- ""
              index<-0
              for(t in 1:length(vrp$tours)){
                tour<-vrp$tours[[t]]
                if(length(tour)>0){
                  index<-index+1
                  vrp$tourenplan <- paste(vrp$tourenplan, 
                                          "Tour:", index,
                                          "\n\tcosts:", tour$costs,
                                          "\n\tloading:", tour$loading,
                                          "\n\tCustomers indices:", paste( tour$stops.indices - M, collapse =" "),
                                          "\n\tTour:", paste(w$label, paste(sapply(tour$stops.list,function(o){o$label}), collapse ="-"),w$label, sep="-"),
                                          "\n") 
                }  
              } 
            
              object$warehouses$vrp[[i]]<-vrp
              totalcosts <- totalcosts + vrp$F
              if(li$log){
                msg<- paste("Tourenplan", w$label)
                message(msg)
                cat(vrp$tourenplan)
              }
            }
            if(li$log) message(paste("Total costs (all Warehouses) are:", totalcosts))
            
            return(object)
          }       
)
