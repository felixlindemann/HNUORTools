#' @name getPhi 
#' @rdname getPhi
#' @aliases Bogenmas
#' @title Calculates the angle of the link between two points to the x-Axis.
#' @description
#' This function calculates the angle of a vector defined by two points
#' in radians. Optionally it can be converted to degrees.
#' For the use of the Operations-Research-Sweep-Algorithm
#' an offset angle can be set.
#' It's als possible return positive values only (2pi will be added).
#'
#' @param x \code{"numeric"} giving the x-Coordinate of the target point
#' @param y \code{"numeric"} giving the y-Coordinate of the target point
#' @param ... See below for optional parameters.
#' @section Optional Parameters (\code{...}):	
#' \subsection{used by getPhi}{
#'    \describe{ 
#'		\item{\code{x0}}{
#'			 \emph{optional} \code{"numeric"}. Representing the x-Coordinate of the start point. \strong{Default} is 0.}
#'		\item{\code{y0}}{
#'			 \emph{optional} \code{"numeric"}. Representing the y-Coordinate of the start point. \strong{Default} is 0.}
#'		\item{\code{nonnegative}}{
#'			 \emph{optional} \code{"logical"}. Indicating if 2pi should be added to negative values. \strong{Default} is \code{TRUE}}
#'		\item{\code{deg}}{
#'			 \emph{optional} \code{"logical"}. Indicating if the result should be converted to degrees. \strong{Default} is \code{FALSE}}
#'		\item{\code{log}}{
#'			 \emph{optional} \code{"logical"}. Indicating if the calculations should be logged. \strong{Default} is \code{FALSE}}
#'		\item{\code{offset}}{
#'			 \emph{optional} \code{"numeric"}. Should be when using rotated coordinate systems. \strong{Default} is 0. \strong{Has to be provided in RADIANS!}}
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{	
#'    \itemize{
#'		\item{\code{...} is currently not forwared.}
#'    }
#' }
#' @keywords radians, angle
#' @export 
#' @return the angle as a value of class \code{"numeric"}.
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
#' Web      www.hs-neu-ulm.de 
#'
#' @references Bronstein
#'
#' @examples
#' # getPhi(1,1,deg=TRUE) # should result 45.
getPhi <- function(x,y,...) UseMethod("getPhi")
getPhi.default<-function(x,y,...){
    li<- list(...)
    value<-NULL
    if(is.null(li$x0) ) {
    	li$x0<-0
    }else{
    	tryCatch(li$x0 <- as.numeric(li$x0), 
    		error = function(e) {
    			stop(paste("Error setting Parameter x0. x0 is not convertable to numeric (",e,")"))
    			})
		
	} 
    if(is.null(li$y0) ) {
    	li$y0<-0
    }else{
		tryCatch(li$y0 <- as.numeric(li$y0), 
    		error = function(e) {
    			stop(paste("Error setting Parameter y0. y0 is not convertable to numeric (",e,")"))
    			})
	} 
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
	if(li$log) cat("calculating angle: (x-x0)/(y-y0):(",x,"-",li$x0,")/(",y,"-",li$y0,")=")
    x<-x-li$x0
	y<-y-li$y0
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
	if(li$log) cat(((x-li$x0)/(y-li$y0))," --> ", value,"\n")


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