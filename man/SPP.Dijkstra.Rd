\docType{methods}
\name{SPP.Dijkstra}
\alias{SPP.Dijkstra}
\alias{SPP.Dijkstra,GeoSituation-method}
\title{Shortest-Path-Problem -- Algorithm of Dijkstra}
\usage{
SPP.Dijkstra(object, ...)

\S4method{SPP.Dijkstra}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{Object of Type \code{\link{GeoSituation}}}

  \item{...}{See below for optional parameters.}
}
\value{
same modified object of Type \code{\link{GeoSituation}}.
The Solution will be assigned the attribute
\code{shortestpath}.
}
\description{
Calculates the Shortest Path from one node to all others in
a network.
}
\details{
This implementation uses the \code{costs} attribute of the
\code{\link{Link}s} provided.
}
\note{
for citing use: Felix Lindemann (2014). HNUORTools:
Operations Research Tools. R package version 1.1-0.
\url{http://felixlindemann.github.io/HNUORTools/}.
}
\section{Optional Parameters (\code{...})}{
  \subsection{used by SPP.Dijkstra}{ \describe{
  \item{\code{log}}{ \emph{optional} \code{"logical"}.
  Indicating if the calculations should be logged.
  \strong{Default} is \code{FALSE}.} \item{\code{debug}}{
  \emph{optional} \code{"logical"}. Indicating if the
  calculations should be debugged \strong{Default} is
  \code{FALSE}. This means, that each process step is
  logged to the console.} \item{\code{start}}{
  \emph{optional} \code{"numeric"}. Indicates which
  \code{\link{Node}}(-index) should be used as startnode.
  \strong{Default} is 1.

  \strong{Has to be a positive 0 < value <= N} (with N =
  the number of nodes in the Scenario.)} } }
  \subsection{Forwarded to the follwowing functions}{
  \itemize{ \item{\code{...} is currently not forwared.} }
  }
}
\examples{
# demo(HNUSPP01)
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
\code{\link{GeoSituation}}, \code{\link{Node}}
}
\keyword{Dijkstra}
\keyword{OR}
\keyword{SPP}
\keyword{Shortest-Path-Problem}

