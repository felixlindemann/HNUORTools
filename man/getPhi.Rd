\name{getPhi}
\alias{Bogenmas}
\alias{getPhi}
\title{Calculates the angle of the link between two points to the x-Axis.}
\usage{
getPhi(x, y, ...)
}
\arguments{
  \item{x}{\code{"numeric"} giving the x-Coordinate of the
  target point}

  \item{y}{\code{"numeric"} giving the y-Coordinate of the
  target point}

  \item{...}{See below for optional parameters.}
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
  \subsection{used by getPhi}{ \describe{ \item{\code{x0}}{
  \emph{optional} \code{"numeric"}. Representing the
  x-Coordinate of the start point. \strong{Default} is 0.}
  \item{\code{y0}}{ \emph{optional} \code{"numeric"}.
  Representing the y-Coordinate of the start point.
  \strong{Default} is 0.} \item{\code{nonnegative}}{
  \emph{optional} \code{"logical"}. Indicating if 2pi
  should be added to negative values. \strong{Default} is
  \code{TRUE}} \item{\code{deg}}{ \emph{optional}
  \code{"logical"}. Indicating if the result should be
  converted to degrees. \strong{Default} is \code{FALSE}}
  \item{\code{log}}{ \emph{optional} \code{"logical"}.
  Indicating if the calculations should be logged.
  \strong{Default} is \code{FALSE}} \item{\code{offset}}{
  \emph{optional} \code{"numeric"}. Should be when using
  rotated coordinate systems. \strong{Default} is 0.
  \strong{Has to be provided in RADIANS!}} } }
  \subsection{Forwarded to the follwowing functions}{
  \itemize{ \item{\code{...} is currently not forwared.} }
  }
}
\examples{
# getPhi(1,1,deg=TRUE) # should result 45.
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
\keyword{angle}
\keyword{radians,}

