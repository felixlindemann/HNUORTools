\docType{methods}
\name{add}
\alias{add}
\alias{add,GeoSituation,Customer-method}
\alias{add,GeoSituation,Link-method}
\alias{add,GeoSituation,Node-method}
\alias{add,GeoSituation,Warehouse-method}
\title{Add an Object to a \code{\link{GeoSituation}}}
\usage{
add(object, value, ...)

\S4method{add}{GeoSituation,Node}(object, value, ...)

\S4method{add}{GeoSituation,Customer}(object, value, ...)

\S4method{add}{GeoSituation,Warehouse}(object, value, ...)

\S4method{add}{GeoSituation,Link}(object, value, ...)
}
\arguments{
  \item{object}{an object of class
  \code{\link{GeoSituation}}}

  \item{value}{an object of class \code{\link{Node}},
  \code{\link{Link}}, \code{\link{Customer}} or
  \code{\link{Warehouse}}}

  \item{...}{Additional argument list that might not ever
  be used.}
}
\value{
The updated object of class \code{\link{GeoSituation}}.
}
\description{
This method should simplify the use of HNUORTools.
}
\note{
for citing use: Felix Lindemann (2014). HNUORTools:
Operations Research Tools. R package version 1.1-0.
\url{http://felixlindemann.github.io/HNUORTools/}.
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

