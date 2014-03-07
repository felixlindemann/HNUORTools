#' @name TPP.MODI
#' @rdname TPP.MODI 
#' @title Transportation-Problem -- MODI-Method -- Modified Distribution Method
#'
#' @description Calculates the Transportationplan.
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TPP.MODI}}}{
#'    \describe{ 
#' 		\item{log}{\code{"logical"} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.} 
#' 		\item{maxiter}{\code{"numeric"} Optional Parameter. if \code{maxiter} is a positive value, the algorithm terminates after \code{maxiter}-iterations.} 
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{TPP.MODI.CalcOppCosts}}}
#'      \item{\code{\link{TPP.SteppingStone.GetBestPolygonZug}}}
#'    }
#' } 
#' @keywords OR TPP Transportation-Problem MODI-Method MODI
#' @export  
#' @return same modified object of Type \code{\link{GeoSituation}}.
#'      The Solution will be assigned the attribute \code{tpp.x}.
#' @references Domschke
#' @seealso \code{\link{GeoSituation}}, \code{\link{Node}}, \code{\link{TPP.NW}}, \code{\link{TPP.CMM}}, \code{\link{TPP.MMM}}, \code{\link{TPP.SteppingStone}}, \code{\link{TPP.MODI}}
#' @examples
#' # demo(HNUTPP03)
#' # demo(HNUTPP04)
#' # demo(HNUTPP05)
#' # demo(HNUTPP06)
#' # demo(HNUTPP07)
#' # demo(HNUTPP08)
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
setGeneric("TPP.MODI",  function(object,...)  standardGeneric("TPP.MODI") ) 
#' @aliases TPP.MODI,GeoSituation-method
#' @rdname TPP.MODI
 setMethod("TPP.MODI", signature(object="GeoSituation"),
  function(object,...){ 
    message("TPP.MODI\n")
  	li <- list(...) 
  	if(is.null(li$maxiter))  li$maxiter  <- -1 
  	if(is.null(li$log)) 	 li$log 	 <- TRUE 

	
	M<- length(object$customers)
 	N<- length(object$warehouses)
 	iter 	 <- 1
	cij <- object$tpp.costs 
 	
 	demand <- sapply(object$customers, function(o){o$demand})
 	supply <- sapply(object$warehouses, function(o){o$supply})
	 
	while( TRUE ){
		#get indices of m.opp
		if(li$log) { message(paste("\tentering iteration:", iter,"\n"))}
		x <-   object$tpp.x 
		if(length(x[x>0]) != M+N -1 ){
			msg<-paste("The current solution is not a base-solution. -> ",
				 "the amount of base-variables is not as expected M+N-1 == length(x[x>0])",
				 "(",(M+N-1),"!= ",length(x[x>0]),").\n",
				 "This problem is not solveable with the current version of ORTools.")
			stop(msg)
		}
		if(sum(x) != sum(demand) ){
			msg <- paste("An error occured in iteration: ",iter, ". The transported amount is not equal to the demand.",
						  "expected: ", sum(demand), "calculated (sum(x)):", sum(x))
			stop(msg)
		}else if( 	sum(x) != sum(supply) ){
			msg <- paste( "An error occured in iteration: ",iter, ". The transported amount is not equal to the supply.",
			 			"expected: ", sum(supply), "calculated (sum(x)):", sum(x))
			stop(msg)
		}

		oldcosts <- sum(x*cij)
  		object <- TPP.MODI.CalcOppCosts(object, ...)
		opp <- object$tpp.costs.opp
		m.opp <- min(opp, na.rm = TRUE)  
		if(li$log) { 
			message(paste("\t\tm.opp:\n")) 
			print(m.opp)
		}
		if(length(m.opp) == 0 | m.opp >= 0 ) {
			message("optimal solution found after ",iter," iteration(s).")
			message("total Costs are: ", oldcosts) 
			break
		}
		Element<-which( opp==m.opp, arr.ind=T )[1,]
		i <- Element[1]
		j <- Element[2]
		Poly<-TPP.SteppingStone.GetBestPolygonZug(object,i=i,j=j,...)# returns
		if(li$log){
			message(paste("\t\tPolygonzug: for Element (",rownames(opp)[i],"/",colnames(opp)[j],")"))
			cat("\t\t\tOpportunity Costs:",Poly$opp," \n") 
			cat("\t\t\tCircle: ") 
			for(k in 1:nrow(Poly$List)){
				cat("(",rownames(opp)[Poly$List$i[k]],"/",
						 colnames(opp)[Poly$List$j[k]],")-")
			}
			k<-1
			cat("(",rownames(opp)[Poly$List$i[k]],"/",
						 colnames(opp)[Poly$List$j[k]],")\n")
		}
		# get Maximum amount to be excanged
		dmin<-max(x)*2 +10 #set upper bound 
		s <- seq(2,length(Poly$List[,2]),by=2)	# define sequence to search
		for(l in s){
			k<-c(Poly$List[l,1],Poly$List[l,2]) # get indices 
			i <- k[1]
			j <- k[2]
			dmin <- max( min(dmin, x[i,j]), 0) # make sure, no negative values are met.
 
		} 
		# do the exchange
		for(l in seq(1,length(Poly$List[,2]))){
			k<-c(Poly$List[l,1],Poly$List[l,2]) # get indices 
			i <- k[1]
			j <- k[2]
			x[i,j]<- x[i,j] + dmin * (-1)^(l+1) # subtract from every second pivot element 
		}
		newcosts <- sum(x*cij)
		if(li$log){
			
			  cat("\t\t\tExchange amount :",dmin," \n") 
			  cat("\t\t\tNew Total Costs :",newcosts,"- improved by ",(oldcosts-newcosts)," (= ",Poly$opp,"*",dmin,") in comparisson to prior iteration.\n") 
			cat(paste("\t\tNew Transportation Plan: \n"))
			print(x)
			cat("------------------- End of Iteration", iter, " ------------------- \n\n")
		}
		
		object$tpp.x <- x 
		iter <- iter+1
		if(li$maxiter >0 & li$maxiter< iter){
			message("Abort by User definition after ",li$maxiter," iteration(s).")	
			break
		} 
	} 
	return(object)
  }  
)