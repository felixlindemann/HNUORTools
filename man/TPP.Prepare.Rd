\docType{methods}
\name{TPP.Prepare}
\alias{TPP.Prepare}
\alias{TPP.Prepare,GeoSituation-method}
\title{Transportation-Problem -- Avoid degenerated Solutions}
\usage{
TPP.Prepare(object, ...)

\S4method{TPP.Prepare}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{Object of Type \code{\link{GeoSituation}}}

  \item{...}{\emph{Optional Parameters} See Below.}
}
\value{
same modified object of Type \code{\link{GeoSituation}}.
The setup may be corrected (a warning will be provided if
so).
}
\description{
prepare the setup of a TPP to be not degenerated. This
function is called automatically when needed. Executing by
User should not required.
}
\note{
for citing use: Felix Lindemann (2014). HNUORTools:
Operations Research Tools. R package version 1.1-0.
\url{http://felixlindemann.github.io/HNUORTools/}.
}
\section{Optional Parameters (\code{...})}{
  \subsection{used by \code{\link{TPP.Prepare}}}{
  \describe{ \item{log}{\code{"logical"} Optional
  Parameter. Indicating, if the calculation should be
  logged to console. Default is \code{FALSE}.}
  \item{checkDegenerated}{\code{"logical"} Optional
  Parameter. Indicating, if the setup should be checked and
  corrected if neccassary. Default value is \code{TRUE}} }
  } \subsection{Forwarded to the follwowing functions}{ You
  may want to check these functions for any other optional
  parameters.  \itemize{ \item{\code{\link{...}}} currently
  not used and not forewarded to other functions.  } }
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
\keyword{OR}
\keyword{TPP}
\keyword{Transportation-Problem}
\keyword{degenerated}
\keyword{solutions}

