\docType{methods}
\name{getDistanceMatrix}
\alias{getDistanceMatrix}
\alias{getDistanceMatrix,GeoSituation,character,character-method}
\title{Calculates the distance matrix for a given origin-destination set}
\usage{
getDistanceMatrix(object, sorigin, sdestination, ...)

\S4method{getDistanceMatrix}{GeoSituation,character,character}(object, sorigin,
  sdestination, ...)
}
\arguments{
  \item{object}{Object of Type \code{\link{GeoSituation}}}

  \item{sorigin}{String taking one of the following values
  Node, Customer, Warehouse}

  \item{sdestination}{String taking one of the following
  values Node, Customer, Warehouse}

  \item{...}{\emph{Optional Parameters} See Below.}
}
\description{
calculates the euklidian-distance. calls the function
\code{\link{getDistance}(...)}
}
\note{
for citing use: Felix Lindemann (2014). HNUORTools:
Operations Research Tools. R package version 1.1-0.
\url{http://felixlindemann.github.io/HNUORTools/}.
}
\section{Optional Parameters (\code{...})}{
  \subsection{used by \code{\link{getDistanceMatrix}}}{
  \describe{ \item{\code{...}:}{no optional parameters are
  used by this function directly.} } }
  \subsection{Forwarded to the follwowing functions}{ You
  may want to check these functions for any other optional
  parameters.  \itemize{
  \item{\code{\link{getDistance}}:}{Parameters are
  forwarded.} } }
}
\examples{
#
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
\seealso{
\code{\link{getDistance}}, code{\link{GeoSituation}},
code{\link{Node}}
}
\keyword{Distance}
\keyword{Euklid}
\keyword{OR}

