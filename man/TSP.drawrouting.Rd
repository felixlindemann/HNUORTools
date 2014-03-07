\docType{methods}
\name{TSP.drawrouting}
\alias{TSP.drawrouting}
\alias{TSP.drawrouting,GeoSituation-method}
\title{Travelling-Salesman-Problem -- drawrouting}
\usage{
TSP.drawrouting(object, ...)

\S4method{TSP.drawrouting}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{Object of Type \code{\link{GeoSituation}}}

  \item{...}{\emph{Optional Parameters} See Below.

  ).}
}
\description{
Draw the current solution in an existing plot.
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
  \subsection{used by TSP.drawrouting}{ \describe{
  \item{startnode.bg.col}{\emph{Optional Parameter} Defines
  the background-color of the start-node. Default value is
  2.} \item{startnode.bg.pch}{\emph{Optional Parameter}
  Defines the background-point-Style of the start-node.
  Default value is 20.}
  \item{startnode.bg.cex}{\emph{Optional Parameter} Defines
  the background-size of the start-node. Default value is
  3.} \item{startnode.fg.col}{\emph{Optional Parameter}
  Defines the foreground-color of the start-node. Default
  value is 1.} \item{startnode.fg.pch}{\emph{Optional
  Parameter} Defines the foreground-point-Style of the
  start-node. Default value is 20.}
  \item{startnode.fg.cex}{\emph{Optional Parameter} Defines
  the foreground-size of the start-node. Default value is
  1.} } } \subsection{Forwarded to the follwowing
  functions}{ You may want to check these functions for any
  other optional parameters.  \itemize{ \item{\code{arrows}
  Arrwos-Parameters \code{x1} and \code{y1} are used by
  \code{TSP.drawrouting}.} } }
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

