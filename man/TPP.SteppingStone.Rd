\docType{methods}
\name{TPP.SteppingStone}
\alias{TPP.SteppingStone}
\alias{TPP.SteppingStone,GeoSituation-method}
\title{Transportation-Problem -- Stepping-Stone-Method}
\usage{
TPP.SteppingStone(object, ...)

\S4method{TPP.SteppingStone}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{Object of Type \code{\link{GeoSituation}}}

  \item{...}{\emph{Optional Parameters} See Below.}
}
\value{
same modified object of Type \code{\link{GeoSituation}}.
The Solution will be assigned the attribute \code{tpp.x}.
}
\description{
Calculates the Transportationplan.
}
\details{
Explain what SSM does.
}
\note{
for citing use: Felix Lindemann (2014). HNUORTools:
Operations Research Tools. R package version 1.1-0.
\url{http://felixlindemann.github.io/HNUORTools/}.
}
\section{Optional Parameters (\code{...})}{
  \subsection{used by \code{\link{TPP.SteppingStone}}}{
  \describe{ \item{log}{\code{"logical"} Optional
  Parameter. Indicating, if the calculation should be
  logged to console. Default is \code{FALSE}.}
  \item{maxiter}{\code{"integer"} Optional Parameter. if
  \code{maxiter} is a positive value, the algorithm
  terminates after \code{maxiter}-iterations.} } }
  \subsection{Forwarded to the follwowing functions}{ You
  may want to check these functions for any other optional
  parameters.  \itemize{
  \item{\code{\link{TPP.SteppingStone.CalcOppMatrix}}}
  \item{\code{\link{TPP.SteppingStone.GetBestPolygonZug}}}
  } }
}
\examples{
# demo(HNUTPP03)
# demo(HNUTPP04)
# demo(HNUTPP05)
# demo(HNUTPP06)
# demo(HNUTPP07)
# demo(HNUTPP08)
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
\keyword{OR}
\keyword{Stepping-Stone}
\keyword{TPP}
\keyword{Transportation-Problem}

