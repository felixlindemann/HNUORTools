\docType{methods}
\name{TSP.3OPT}
\alias{TSP.3OPT}
\alias{TSP.3OPT,GeoSituation-method}
\title{Travelling-Salesman-Problem -- Three-Opt}
\usage{
TSP.3OPT(object, ...)

\S4method{TSP.3OPT}{GeoSituation}(object, ...)
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
Explain what Three-opt does.
}
\note{
for citing use: Felix Lindemann (2014). HNUORTools:
Operations Research Tools. R package version 1.1-0.
\url{http://felixlindemann.github.io/HNUORTools/}.
}
\section{Optional Parameters (\code{...})}{
  \subsection{used by \code{\link{TSP.3OPT}}}{ \describe{
  \item{log}{\code{"logical"} Optional Parameter.
  Indicating, if the calculation should be logged to
  console. Default is \code{FALSE}.} } }
  \subsection{Forwarded to the follwowing functions}{ You
  may want to check these functions for any other optional
  parameters.  \itemize{ \item{\code{\link{...}} currently
  not forewarded.} } }
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
\keyword{3-opt}
\keyword{OR}
\keyword{TSP}
\keyword{Three-opt}
\keyword{Travelling-Salesman-Problem}

