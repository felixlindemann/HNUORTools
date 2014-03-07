\docType{methods}
\name{TPP.SteppingStone.GetPolygonZuege}
\alias{TPP.SteppingStone.GetPolygonZuege}
\alias{TPP.SteppingStone.GetPolygonZuege,GeoSituation,integer,integer-method}
\title{Transportation-Problem -- Stepping-Stone-Method -- Helper: GetPolygonZuege}
\usage{
TPP.SteppingStone.GetPolygonZuege(object, i_i, j_j, ...)


  \S4method{TPP.SteppingStone.GetPolygonZuege}{GeoSituation,integer,integer}(object,
  i_i, j_j, ...)
}
\arguments{
  \item{object}{Object of Type \code{\link{GeoSituation}}}

  \item{i_i}{integer REQUIRED. Describes the pivot-Element
  the polygon-cycles should be searched for.}

  \item{j_j}{integer REQUIRED. Describes the pivot-Element
  the polygon-cycles should be searched for.}

  \item{...}{\emph{Optional Parameters} See Below.}
}
\description{
Calculates all Polygon-Cycles.
}
\details{
Explain what SSM does.
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
  \subsection{used by
  \code{\link{TPP.SteppingStone.GetPolygonZuege}}}{
  \describe{ \item{log}{\code{"logical"} Optional
  Parameter. Indicating, if the calculation should be
  logged to console. Default is \code{FALSE}.}
  \item{Poly}{\code{"list"} \emph{Optional Parameter}. Used
  only if the function is called recursively.} } }
  \subsection{Forwarded to the follwowing functions}{ You
  may want to check these functions for any other optional
  parameters.  \itemize{
  \item{\code{\link{TPP.SteppingStone.CalcPolyGonZug}}}
  \item{\code{\link{TPP.SteppingStone.GetPolygonZuege}}} }
  }
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

