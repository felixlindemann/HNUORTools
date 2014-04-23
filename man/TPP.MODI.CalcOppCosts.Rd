\docType{methods}
\name{TPP.MODI.CalcOppCosts}
\alias{TPP.MODI.CalcOppCosts}
\alias{TPP.MODI.CalcOppCosts,GeoSituation-method}
\title{Transportation-Problem -- MODI-Method -- Helper: CalcOppMatrix}
\usage{
TPP.MODI.CalcOppCosts(object, ...)

\S4method{TPP.MODI.CalcOppCosts}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{Object of Type \code{\link{GeoSituation}}}

  \item{...}{\emph{Optional Parameters} See Below.}
}
\description{
Calculates the Opportunity-Cost-Matrix
}
\details{
Explain what MODI does.
}
\note{
for citing use: Felix Lindemann (2014). HNUORTools:
Operations Research Tools. R package version 1.1-0.
\url{http://felixlindemann.github.io/HNUORTools/}.
}
\section{WARNING}{
  This function is meant for internal purposes only. It is
  not required to call this function manually at any time!
}

\section{Optional Parameters (\code{...})}{
  \subsection{used by \code{\link{TPP.MODI.CalcOppCosts}}}{
  \describe{ \item{log}{logical Optional Parameter.
  Indicating, if the calculation should be logged to
  console. Default is \code{FALSE}.} } }
  \subsection{Forwarded to the follwowing functions}{ You
  may want to check these functions for any other optional
  parameters.  \itemize{
  \item{\code{\link{getInitialMatrix}}} } }
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

