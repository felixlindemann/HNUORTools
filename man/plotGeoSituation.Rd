\docType{methods}
\name{plotGeoSituation}
\alias{plotGeoSituation}
\alias{plotGeoSituation,GeoSituation-method}
\title{plotGeoSituation}
\usage{
plotGeoSituation(object, ...)

\S4method{plotGeoSituation}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{the \code{\link{GeoSituation}-Object} to be
  plotted}

  \item{...}{list of optional values (see below)}
}
\description{
plotGeoSituation
}
\details{
Optional Parameters for this Methdod.
}
\section{optional arguments}{
  \describe{

  \item{ as used in \code{plot.default}:}{ The parameters
  \code{xlim}, \code{ylim}, \code{xlab}, \code{ylab},
  \code{main}, \code{sub} recieve a default value, if not
  given. By default \code{asp=1} is set and should not be
  changed.  } \item{\code{plotBorders}:}{
  \code{\link{logical}} default = \code{FALSE}. If
  \code{TRUE}, the borders from the dataset
  bordersgermany.polygon will be drawn.  }
  \item{\code{plotcities}:}{ \code{\link{logical}} default
  = \code{FALSE}. If \code{TRUE}, the cities from the
  dataset bordersgermany.cities will be drawn.  }
  \item{\code{plotGrid}:}{ \code{\link{logical}} default =
  \code{FALSE}. If \code{TRUE}, a grid will be drawn using
  the function \code{\link{grid}}.  }
  \item{\code{drawNodes}:}{ \code{\link{logical}} default =
  \code{TRUE}. If \code{TRUE}, all \code{\link{Node}s} in
  \code{x (of type \link{GeoSituation}} will be plotted.  }
  \item{\code{drawWarehouses}:}{ \code{\link{logical}}
  default = \code{TRUE}. If \code{TRUE}, all
  \code{\link{Warehouse}s} in \code{x (of type
  \link{GeoSituation}} will be plotted.  }
  \item{\code{drawCustomers}:}{ \code{\link{logical}}
  default = \code{TRUE}. If \code{TRUE}, all
  \code{\link{Customer}s} in \code{x (of type
  \link{GeoSituation}} will be plotted by calling
  \code{\link{drawCustomers}}.  } \item{\code{drawLinks}:}{
  \code{\link{logical}} default = \code{TRUE}. If
  \code{TRUE}, all \code{\link{Link}s} in \code{x (of type
  \link{GeoSituation}} will be plotted.  } }
}

