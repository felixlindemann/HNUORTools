\docType{methods}
\name{TPP.NW}
\alias{TPP.NW}
\alias{TPP.NW,GeoSituation-method}
\title{Transportation-Problem -- North-West-Corner-Rule}
\usage{
TPP.NW(object, ...)

\S4method{TPP.NW}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{Object of Type \code{\link{GeoSituation}}}

  \item{...}{\emph{Optional Parameters} See Below.}
}
\description{
Calculates the Transportationplan.
}
\note{
for citing use: Felix Lindemann (2014). HNUORTools:
Operations Research Tools. R package version 1.1-0.
\url{http://felixlindemann.github.io/HNUORTools/}.
}
\section{Optional Parameters (\code{...})}{
  \subsection{used by \code{\link{TPP.NW}}}{ \describe{
  \item{log}{\code{"logical"} Optional Parameter.
  Indicating, if the calculation should be logged to
  console. Default is \code{FALSE}.} } }
  \subsection{Forwarded to the follwowing functions}{ You
  may want to check these functions for any other optional
  parameters.  \itemize{
  \item{\code{\link{getInitialMatrix}}}
  \item{\code{\link{TPP.Prepare}}} } }
}
\examples{
# demo(HNUTPP03)
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
\code{\link{TPP.NW}}, \code{\link{TPP.CMM}},
\code{\link{TPP.MMM}}, \code{\link{TPP.SteppingStone}},
\code{\link{TPP.MODI}}
}
\keyword{North-West-Corner-Rule}
\keyword{OR}
\keyword{TPP}
\keyword{Transportation-Problem}

