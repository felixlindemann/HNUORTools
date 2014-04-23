#' @aliases getpolar,numeric,numeric-method 
#' @rdname getpolar  
#' @export
#' @examples
#' # getpolar(c(1,1),c(0,0),deg=TRUE) # should result 45. 
setMethod( "getpolar", signature =c("numeric", "numeric"),
	function(n0=c(0,0),n1,...){
	    li<- list(...)

	    if(length(n0)!=2){stop(paste("Argument n0 is not as expected of length 2. (Obtained:",length(n0),")",sep=""))}
		if(length(n1)!=2){stop(paste("Argument n1 is not as expected of length 2. (Obtained:",length(n1),")",sep=""))}
		
		x1 <- n1[1]
		x0 <- n0[1]
	    y1 <- n1[2]
		y0 <- n0[2]
	    value<-NULL

	    if(is.null(li$deg)) {
	    	li$deg = FALSE
		}else{
			if(!class(li$deg) == "logical")
				stop("Parameter deg must be of type logical (boolean)")
		}
	    if(is.null(li$nonnegative)) {
	    	li$nonnegative = TRUE
	    }else{
			if(!class(li$nonnegative) == "logical")
				stop("Parameter nonnegative must be of type logical (boolean)")
		}
	    if(is.null(li$offset)) {
	    	li$offset <- 0
		}else{
			li$offset <- as.numeric(li$offset)
		}
	    if(is.null(li$log)) {
	    	li$log = FALSE
		}else{
			if(!class(li$log) == "logical")
				stop("Parameter log must be of type logical (boolean)")
		}
		if(li$log) cat("calculating angle: (x-x0)/(y-y0):(",x1,"-",x0,")/(",y1,"-",y0,")=")
	    x<-x1-x0
		y<-y1-y0
		if(x==0){
			if(y>0){
				value<-pi/2
			}else{
				if(y<0){
					value<- 3*pi/2
				}else{
					warning("Angle can't be calculated because both coordinates are the same. Returning NA.")
					return(NA)
				}
			}
		}else{
			value<-atan(y/x)
			if(x<0){
				value<-value+pi
			} 
		}
		if(li$log) cat(((x-x0)/(y-y0))," --> ", value,"\n")


		value <- value - li$offset
		if(li$log & li$offset !=0) cat("\tSubtracting offset: (",li$offset,")= ", value, "\n") 
		if(li$nonnegative){		
			while(TRUE){
				if(value>=0) break			
				value<-value+2*pi
				if(li$log ) cat("\tadding 2pi: ",value,"\n") 
			}
		}

		if(li$deg){
			value<-value/pi*180
			if(li$log ) cat("\tconverting to degrees: ",value,"\n") 
		}
	    return(value)
	}
) 











