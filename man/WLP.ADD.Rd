\docType{methods}
\name{WLP.ADD}
\alias{WLP.ADD}
\alias{WLP.ADD,GeoSituation-method}
\title{Warehouse-Location-Problem -- ADD-Algorithm}
\usage{
WLP.ADD(object, ...)

\S4method{WLP.ADD}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{Object of Type \code{\link{GeoSituation}}}

  \item{...}{\emph{Optional Parameters} See Below.}
}
\value{
same modified object of Type \code{\link{GeoSituation}}.
The Solution will be assigned the attribute
\code{WLP.solution}.
}
\description{
Improves a given Route by switching links.
}
\details{
Explain what ADD-Algorithm does.
}
\note{
for citing use: Felix Lindemann (2014). HNUORTools:
Operations Research Tools. R package version 1.1-0.
\url{http://felixlindemann.github.io/HNUORTools/}.
}
\section{Optional Parameters (\code{...})}{
  \subsection{used by \code{\link{WLP.ADD}}}{ \describe{
  \item{log}{\code{"logical"} Optional Parameter.
  Indicating, if the calculation should be logged to
  console. Default is \code{FALSE}.}
  \item{maxiter}{\code{"numeric"} Optional Parameter. if
  \code{maxiter} is a positive value, the algorithm
  terminates after \code{maxiter}-iterations.}
  \item{cij}{\code{"matrix"} \emph{Optional Parameter}. use
  for a user-defined cij-matrix.} } } \subsection{Forwarded
  to the follwowing functions}{ You may want to check these
  functions for any other optional parameters.  \itemize{
  \item{\code{\link{...}} currently not used and not
  forewarded. } } }
}
\examples{
# demo(HNUWLP01)
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
\code{\link{GeoSituation}},
\code{\link{Node}},\code{\link{Warehouse}},\code{\link{Customer}},
\code{\link{WLP.ADD}}
}
\keyword{ADD-Algorithm}
\keyword{OR}
\keyword{WLP}
\keyword{Warehouse-Location-Problem}

