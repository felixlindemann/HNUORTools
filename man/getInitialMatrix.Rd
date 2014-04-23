\docType{methods}
\name{getInitialMatrix}
\alias{getInitialMatrix}
\alias{getInitialMatrix,GeoSituation,character,character-method}
\alias{getInitialMatrix,GeoSituation-method}
\title{Caclulate Inital-Matrix}
\usage{
getInitialMatrix(object, ...)

\S4method{getInitialMatrix}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{Object of Type \code{\link{GeoSituation}}}

  \item{...}{\emph{Optional Parameters} See Below.}
}
\description{
Calculates an inital MxN-matrix (Warehouses x Customers)
}
\note{
for citing use: Felix Lindemann (2014). HNUORTools:
Operations Research Tools. R package version 1.1-0.
\url{http://felixlindemann.github.io/HNUORTools/}.
}
\section{Optional Parameters (\code{...})}{
  \subsection{used by \code{\link{getInitialMatrix}}}{
  \describe{ \item{initialvalue}{\code{ANY} Optional
  Parameter for initial value. Default is \code{NA}.} } }
  \subsection{Forwarded to the follwowing functions}{ You
  may want to check these functions for any other optional
  parameters.  \itemize{ \item{\code{\link{...}} currently
  not forewarded.} } }
}
\examples{
#
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
\seealso{
\code{\link{GeoSituation}}, \code{\link{Customer}},
\code{\link{Warehouse}}
}
\keyword{Initial-MAtrix}
\keyword{OR}

