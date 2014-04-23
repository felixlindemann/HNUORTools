\docType{methods}
\name{drawWarehouses}
\alias{drawWarehouses}
\alias{drawWarehouses,GeoSituation-method}
\title{drawWarehouses}
\usage{
drawWarehouses(object, ...)

\S4method{drawWarehouses}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{the \code{\link{GeoSituation}-Object(s)} to
  be plotted}

  \item{...}{list of optional values (see below)}
}
\description{
drawWarehouses
}
\details{
Optional Parameters for this Methdod.
}
\section{optional arguments}{
  \describe{ \item{\code{zoom}:}{ \code{\link{numeric}}
  default = \code{0.1}. Used as a scale for the
  House-Polygons.  } \item{\code{withlabels}:}{
  \code{\link{logical}} default = \code{TRUE}. If
  \code{TRUE}, labels will be printed for each warehouse.
  } \item{\code{isWLP}:}{ \code{\link{logical}} default =
  \code{FALSE}. If \code{TRUE}, closed Warehouses will be
  printed blank.  } \item{\code{warehouse.font.cex}:}{
  \code{\link{numeric}} default = 1. Defines with which
  font-size the labels will be printed.  }
  \item{\code{warehouse.font.col}:}{ \code{\link{numeric}
  or \link{character}} default = \code{"black"}. The color
  the labels should be printed in.  Can be an Array of
  length of Warehouses to provide different colors. By
  default, the provided value will be copied.  }
  \item{\code{warehouse.bg.col}:}{ \code{\link{numeric} or
  \link{character}} default = \code{"gray"}. The color the
  warehouses-background should be printed in.  Can be an
  Array of length of Warehouses to provide different
  colors. By default, the provided value will be copied.  }
  \item{\code{warehouse.borders.col}:}{
  \code{\link{numeric} or \link{character}} default =
  \code{"black"}. The color the warehouses-outline should
  be printed in.  Can be an Array of length of Warehouses
  to provide different colors. By default, the provided
  value will be copied.  }
  \item{\code{warehouse.point.pch}:}{ \code{\link{numeric}}
  default = \code{20}. defines the pch of the warehouses.
  can be an array of length of warehouses.  By default, the
  provided value will be copied.  }
  \item{\code{warehouse.point.cex}:}{ \code{\link{numeric}}
  default = \code{1}. defines the cex of the warehouses.
  can be an array of length of warehouses.  By default, the
  provided value will be copied.  } }
}

