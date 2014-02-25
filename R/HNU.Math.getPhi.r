
HNU.Math.getPhi <- function(x,y,...) UseMethod("HNU.Math.getPhi")
HNU.Math.getPhi.default<-function(x,y,...){
    li<- list(...)
    value<-NULL
    if(is.null(li$x0) ) li$x0<-0
    if(class(li$x0)!="numeric"){
    	stop("x0 must be a numeric")
    }
    if(is.null(li$x1) ) li$x1<-0
    if(class(li$x1)!="numeric"){
    	stop("x1 must be a numeric")
    }
    if(is.null(li$deg)) li$deg = FALSE
    if(is.null(li$nonnegative)) li$nonnegative = TRUE
    if(is.null(li$startangle)) li$startangle <- 0

    x<-x-x0
	y<-y-y0
	if(x==0){
		if(y>0){
			value<-pi/2
		}else{
			if(y<0){
				value<- 3*pi/2
			}else{
				value<-NA
			}
		}
	}else{
		value<-atan(y/x)
		if(x<0){
			value<-value+pi
		} 
	}
	value <- value - li$startangle

	if(li$nonnegative){
		while(value <0){
			value<-value+2*pi
		}
	}

	if(li$deg){
		value<-value/pi*180
	}
    return(value)
} 