\docType{methods}
\name{VRP.SWEEP}
\alias{VRP.SWEEP}
\alias{VRP.SWEEP,GeoSituation-method}
\title{Vehicle-Routing-Problem -- SWEEP-Algorithm}
\usage{
VRP.SWEEP(object, ...)

\S4method{VRP.SWEEP}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{Object of Type \code{\link{GeoSituation}}}

  \item{...}{\emph{Optional Parameters} See Below.}
}
\value{
same modified object of Type \code{\link{GeoSituation}}.
The Solution will be assigned to each
\code{\link{Warehouse}$vrp}
}
\description{
Calculate solution for the VRP using the SWEEP-Algorithm.
}
\details{
Explain what VRP.SWEEP does.
}
\note{
for citing use: Felix Lindemann (2014). HNUORTools:
Operations Research Tools. R package version 1.1-0.
\url{http://felixlindemann.github.io/HNUORTools/}.
}
\section{Optional Parameters (\code{...})}{
  \subsection{used by \code{\link{VRP.SWEEP}}}{ \describe{
  \item{alpha}{ numeric The \code{alpha}-Shape-Parameter of
  the SWEEP-algorithm (Offset of Sweep-Angle. To be rovided
  in Radians!)} \item{vehiclecapacity}{ numeric
  \emph{Optional Parameter}. Defining the maximum
  loading-capacity of each tour. Default is 2*sum(demand)
  +1 (will be ignored).}
  \item{vehiclecapacity.maxstops}{numeric \emph{Optional
  Parameter}. Defining the maximum Stops of each tour.
  Default is 2*n +1 (will be ignored).} \item{roundcij}{
  logical Optional Parameter. Indicating, if the calculated
  costs should be round. Default is \code{TRUE}.}
  \item{log}{logical Optional Parameter. Indicating, if the
  calculation should be logged to console. Default is
  \code{FALSE}.} } } \subsection{Forwarded to the
  follwowing functions}{ You may want to check these
  functions for any other optional parameters.  \itemize{
  \item{\code{\link{calc.polar}} (offset-parameter is used
  by \code{\link{VRP.SWEEP}}} } }
}
\examples{
# demo(HNUVRP01)
# demo(HNUVRP02)
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
\code{\link{VRP.SWEEP}} \code{\link{VRP.SAVINGS}}
}
\keyword{OR}
\keyword{SWEEP}
\keyword{VRP}
\keyword{Vehicle-Routing-Problem}

