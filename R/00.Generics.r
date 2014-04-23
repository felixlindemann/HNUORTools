#' @name getpolar 
#' @rdname getpolar 
#' @title Calculates the angle of the link between two points to the x-Axis.
#' @description
#' This function calculates the angle of a vector defined by two points
#' in radians. Optionally it can be converted to degrees.
#' For the use of the Operations-Research-Sweep-Algorithm
#' an offset angle can be set.
#' It's als possible return positive values only (2pi will be added).
#' @param n0 an object of class \code{\link{numeric}} with a length of 2 (x/y-coordinates) or a class derived from \code{\link{Node}}. Used as origin.
#' @param n1 an object of class \code{\link{numeric}} with a length of 2 (x/y-coordinates) or a class derived from \code{\link{Node}}.
#' @param ... see \emph{Optional Values}
#' @section Optional Parameters (\code{...}):	
#' \subsection{used by getpolar}{
#'    \describe{  
#'		\item{\code{nonnegative}}{
#'			 \emph{optional} \code{\link{logical}}. Indicating if 2pi should be added to negative values. \strong{Default} is \code{TRUE}}
#'		\item{\code{deg}}{
#'			 \emph{optional} \code{\link{logical}}. Indicating if the result should be converted to degrees. \strong{Default} is \code{FALSE}}
#'		\item{\code{log}}{
#'			 \emph{optional} \code{\link{logical}}. Indicating if the calculations should be logged. \strong{Default} is \code{FALSE}}
#'		\item{\code{offset}}{
#'			 \emph{optional} \code{\link{numeric}}. Should be when using rotated coordinate systems. \strong{Default} is 0. \strong{Has to be provided in RADIANS!}}
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{	
#'    \itemize{
#'		\item{\code{...} is currently not forwared.}
#'    }
#' }
#'
#' @keywords radians, polar-angle, polar, coordinates
#' @export 
#' @return the angle as a value of class  \code{\link{numeric}}.
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
#' @references      
#'Bronstein 2008 Bronstein, I. N.: Taschenbuch der Mathematik.
#'[CD-ROM-Ausg. der 7., voll. ueberarb. und erg. gedr. Ausg.].
#'Frankfurt:  Harri Deutsch, 2008 
setGeneric("getpolar", function(n0,n1,...) standardGeneric("getpolar")) 

#' @title distance between two \code{\link{Node}s}
#' @description Calculates the euclidian distance between to nodes. Optional this distance can be multiplied by a  \code{costfactor} in order to convert the distance to cost-related value (such as time, costs, ...).
#' \deqn{ f(x) = \sqrt((x_0-x_1)^2 + (y_0-y_1)^2) \cdot costfactor }{ costfactor * ((x_0-x_1)^2 + (y_0-y_1)^2)^0.5  }
#' @param n0 an object of class \code{\link{numeric}} with a length of 2 (x/y-coordinates) or a class derived from \code{\link{Node}}. Used as origin.
#' @param n1 an object of class \code{\link{numeric}} with a length of 2 (x/y-coordinates) or a class derived from \code{\link{Node}}.
#' @param ... \emph{Optional Parameters} (See Below).
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{getDistance}}}{
#'    \describe{ 
#'     \item{digits}{non negative Integer value for the number of digitis that has to be rounded to.}  
#'      \item{costfactor}{ \code{\link{numeric}} Optional Parameter. Used to transform the distance by a factor into costs. Default is \code{1}.}  
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{round}} called, if the parameter \code{digits} is provided.} 
#'    }
#' } 
#' @name getDistance 
#' @docType methods
#' @rdname getDistance
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
setGeneric("getDistance", function(n0,n1,   ...) standardGeneric("getDistance"))


 
