\docType{methods}
\name{TPP.VOGEL}
\alias{TPP.VOGEL}
\alias{TPP.VOGEL,GeoSituation-method}
\title{Transportation-Problem -- VogelAproximation Method}
\usage{
TPP.VOGEL(object, ...)

\S4method{TPP.VOGEL}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{Object of Type \code{\link{GeoSituation}}}

  \item{...}{\emph{Optional Parameters} See Below.}
}
\value{
same (but modified) object of Type
\code{\link{GeoSituation}}.  The Solution will be assigned
the attribute \code{tpp.x}.
}
\description{
Calculates the Transportation-Plan.
}
\note{
for citing use: Felix Lindemann (2014). HNUORTools:
Operations Research Tools. R package version 1.1-0.
\url{http://felixlindemann.github.io/HNUORTools/}.
}
\section{Optional Parameters (\code{...})}{
  \subsection{used by \code{\link{TPP.VOGEL}}}{ \describe{
  \item{log}{logical Optional Parameter. Indicating, if the
  calculation should be logged to console. Default is
  \code{FALSE}.}

  } } \subsection{Forwarded to the follwowing functions}{
  You may want to check these functions for any other
  optional parameters.  \itemize{
  \item{\code{\link{getInitialMatrix}}}
  \item{\code{\link{TPP.Prepare}}}
  \item{\code{\link{TPP.getCostMatrix}}} } }
}
\examples{
# demo(HNUTPP02)
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
Domschke, Wolfgang; Drexl, Andreas (2005): Einfuehrung in
Operations Research. Mit 63 Tabellen. 6., ueberarb. und
erw. Aufl. Berlin: Springer.
}
\seealso{
\code{\link{GeoSituation}}, \code{\link{Node}},
\code{\link{TPP.NW}}, \code{\link{TPP.VOGEL}},
\code{\link{TPP.MMM}}, \code{\link{TPP.SteppingStone}},
\code{\link{TPP.MODI}}
}
\keyword{Column-Minimum-Method}
\keyword{OR}
\keyword{TPP}
\keyword{Transportation-Problem}

