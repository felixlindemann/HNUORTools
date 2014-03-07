\docType{methods}
\name{getpolar}
\alias{getpolar}
\alias{getpolar,Node,Node-method}
\alias{getpolar,numeric,numeric-method}
\title{Calculates the angle of the link between two points to the x-Axis.}
\usage{
getpolar(n0, n1, ...)

\S4method{getpolar}{numeric,numeric}(n0 = c(0, 0), n1, ...)

\S4method{getpolar}{Node,Node}(n0, n1, ...)
}
\arguments{
  \item{n0}{an object of class \code{"numeric"} with a
  length of 2 (x/y-coordinates) or a class derived from
  \code{"Node"}. Used as origin.}

  \item{n1}{an object of class \code{"numeric"} with a
  length of 2 (x/y-coordinates) or a class derived from
  \code{"Node"}.}

  \item{...}{see \emph{Optional Values}}
}
\value{
the angle as a value of class \code{"numeric"}.
}
\description{
This function calculates the angle of a vector defined by
two points in radians. Optionally it can be converted to
degrees. For the use of the
Operations-Research-Sweep-Algorithm an offset angle can be
set. It's als possible return positive values only (2pi
will be added).
}
\section{Optional Parameters (\code{...})}{
  \subsection{used by getpolar}{ \describe{
  \item{\code{nonnegative}}{ \emph{optional}
  \code{"logical"}. Indicating if 2pi should be added to
  negative values. \strong{Default} is \code{TRUE}}
  \item{\code{deg}}{ \emph{optional} \code{"logical"}.
  Indicating if the result should be converted to degrees.
  \strong{Default} is \code{FALSE}} \item{\code{log}}{
  \emph{optional} \code{"logical"}. Indicating if the
  calculations should be logged. \strong{Default} is
  \code{FALSE}} \item{\code{offset}}{ \emph{optional}
  \code{"numeric"}. Should be when using rotated coordinate
  systems. \strong{Default} is 0. \strong{Has to be
  provided in RADIANS!}} } } \subsection{Forwarded to the
  follwowing functions}{ \itemize{ \item{\code{...} is
  currently not forwared.} } }
}
\examples{
# getpolar(c(1,1),c(0,0),deg=TRUE) # should result 45.
# w1 <- new("Warehouse", x=0, y=0)
# c1 <- new("Customer",  x=1, y=1)
# getpolar(w1,c1,deg=TRUE) # should result 45.
}
\author{
Dipl. Kfm. Felix Lindemann
\email{felix.lindemann@hs-neu-ulm.de}

Wissenschaftlicher Mitarbeiter Kompetenzzentrum Logistik
Buro ZWEI, 17

Hochschule fur angewandte Wissenschaften Fachhochschule
Neu-Ulm | Neu-Ulm University Wileystr. 1

D-89231 Neu-Ulm

Phone +49(0)731-9762-1437 Web www.hs-neu-ulm.de
}
\references{
Bronstein
}
\keyword{coordinates}
\keyword{polar,}
\keyword{polar-angle,}
\keyword{radians,}

