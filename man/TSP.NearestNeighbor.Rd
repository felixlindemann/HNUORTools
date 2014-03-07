\docType{methods}
\name{TSP.NearestNeighbor}
\alias{TSP.NearestNeighbor}
\alias{TSP.NearestNeighbor,GeoSituation-method}
\title{Travelling-Salesman-Problem -- NearestNeighbor}
\usage{
TSP.NearestNeighbor(object, ...)

\S4method{TSP.NearestNeighbor}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{Object of Type \code{\link{GeoSituation}}}

  \item{...}{\emph{Optional Parameters} See Below.}
}
\value{
same modified object of Type \code{\link{GeoSituation}}.
The Solution will be assigned the attribute
\code{tsp.solution}.
}
\description{
Improves a given Route by switching links.
}
\details{
Explain what NearestNeighbor does.
}
\note{
for citing use: Felix Lindemann (2014). HNUORTools:
Operations Research Tools. R package version 1.1-0.
\url{http://felixlindemann.github.io/HNUORTools/}.
}
\section{Optional Parameters (\code{...})}{
  \subsection{used by \code{\link{TSP.NearestNeighbor}}}{
  \describe{ \item{log}{\code{logical} Optional Parameter.
  Indicating, if the calculation should be logged to
  console. Default is \code{FALSE}.}
  \item{alpha}{\code{numeric} The
  \code{alpha}-Shape-Parameter of the savings-algorithm.
  Default is \code{1}} \item{calcCij}{\code{logical}
  \emph{Optional Parameter}. Indicating, if the cost matrix
  cij has to be calculated. Default is \code{TRUE}.}
  \item{cij}{\code{matrix} \emph{Optional Parameter}. Use
  for providing a user-defined cij-matrix. By default it's
  calculated.} } } \subsection{Forwarded to the follwowing
  functions}{ You may want to check these functions for any
  other optional parameters.  \itemize{
  \item{\code{\link{TPP.SteppingStone.GetPolygonZuege}}} }
  }
}
\examples{
# demo(HNUTSP01)
# demo(HNUTSP02)
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
\references{
Domschke
}
\seealso{
\code{\link{GeoSituation}}, \code{\link{Node}},
\code{\link{TSP.NearestNeighbor}}, \code{\link{TSP.2OPT}},
\code{\link{TSP.3OPT}}
}
\keyword{NearestNeighbor}
\keyword{OR}
\keyword{TSP}
\keyword{Travelling-Salesman-Problem}

