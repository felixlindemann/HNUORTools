\docType{methods}
\name{getDistance}
\alias{getDistance}
\alias{getDistance,Node,Node-method}
\alias{getDistance,numeric,numeric-method}
\title{distance between two \code{\link{Node}s}}
\usage{
getDistance(n0, n1, ...)

\S4method{getDistance}{numeric,numeric}(n0 = c(0, 0), n1, ...)

\S4method{getDistance}{Node,Node}(n0, n1, ...)
}
\arguments{
  \item{n0}{an object of class \code{\link{numeric}} with a
  length of 2 (x/y-coordinates) or a class derived from
  \code{\link{Node}}. Used as origin.}

  \item{n1}{an object of class \code{\link{numeric}} with a
  length of 2 (x/y-coordinates) or a class derived from
  \code{\link{Node}}.}

  \item{...}{\emph{Optional Parameters} (See Below).}
}
\description{
Calculates the euclidian distance between to nodes.
Optional this distance can be multiplied by a
\code{costfactor} in order to convert the distance to
cost-related value (such as time, costs, ...). \deqn{ f(x)
= \sqrt((x_0-x_1)^2 + (y_0-y_1)^2) \cdot costfactor }{
costfactor * ((x_0-x_1)^2 + (y_0-y_1)^2)^0.5 }
}
\section{Optional Parameters (\code{...})}{
  \subsection{used by \code{\link{getDistance}}}{
  \describe{ \item{digits}{non negative Integer value for
  the number of digitis that has to be rounded to.}
  \item{costfactor}{ \code{\link{numeric}} Optional
  Parameter. Used to transform the distance by a factor
  into costs. Default is \code{1}.} } }
  \subsection{Forwarded to the follwowing functions}{ You
  may want to check these functions for any other optional
  parameters.  \itemize{ \item{\code{\link{round}} called,
  if the parameter \code{digits} is provided.} } }
}
\examples{
# getDistance(c(1,1),c(4,5),costfactor=1) # should result 5.
# getDistance(c(1,1),c(4,5),costfactor=2) # should result 10.
# w1 <- new("Node", x=0, y=0)
# c1 <- new("Node",  x=3, y=4)
# getDistance(w1,c1) # should result 5.
# getDistance(w1,c1,costfactor=3) # should result 15.
}
\author{
Dipl. Kfm. Felix Lindemann
\email{felix.lindemann@hs-neu-ulm.de}

Wissenschaftlicher Mitarbeiter Kompetenzzentrum Logistik
Buro ZWEI, 17

Hochschule fur angewandte Wissenschaften Fachhochschule
Neu-Ulm | Neu-Ulm University Wileystr. 1

D-89231 Neu-Ulm

Phone +49(0)731-9762-1437 Web
\url{www.hs-neu-ulm.de/felix-lindemann/}
\url{http://felixlindemann.blogspot.de}
}

