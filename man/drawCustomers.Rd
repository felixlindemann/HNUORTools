\docType{methods}
\name{drawCustomers}
\alias{drawCustomers}
\alias{drawCustomers,GeoSituation-method}
\title{drawCustomers}
\usage{
drawCustomers(object, ...)

\S4method{drawCustomers}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{the \code{\link{GeoSituation}-Object(s)} to
  be plotted}

  \item{...}{list of optional values (see below)}
}
\description{
drawCustomers
}
\details{
Optional Parameters for this Methdod.
}
\section{optional arguments}{
  \describe{ \item{\code{zoom}:}{ \code{\link{numeric}}
  default = \code{0.1}. Used as a scale for the
  House-Polygons.  } \item{\code{withlabels}:}{
  \code{\link{logical}} default = \code{TRUE}. If
  \code{TRUE}, labels will be printed for each customer.  }
  \item{\code{customer.font.cex}:}{ \code{\link{numeric}}
  default = 1. Defines with which font-size the labels will
  be printed.  } \item{\code{customer.font.col}:}{
  \code{\link{numeric} or \link{character}} default =
  \code{"black"}. The color the labels should be printed
  in.  Can be an Array of length of Customers to provide
  different colors. By default, the provided value will be
  copied.  } \item{\code{customer.bg.col}:}{
  \code{\link{numeric} or \link{character}} default =
  \code{"gray"}. The color the warehouses-background should
  be printed in.  Can be an Array of length of Customers to
  provide different colors. By default, the provided value
  will be copied.  } \item{\code{customer.borders.col}:}{
  \code{\link{numeric} or \link{character}} default =
  \code{"black"}. The color the warehouses-outline should
  be printed in.  Can be an Array of length of Customers to
  provide different colors. By default, the provided value
  will be copied.  } \item{\code{customer.point.pch}:}{
  \code{\link{numeric}} default = \code{20}. defines the
  pch of the customers. can be an array of length of
  customers.  By default, the provided value will be
  copied.  } \item{\code{customer.point.cex}:}{
  \code{\link{numeric}} default = \code{1}. defines the cex
  of the customers. can be an array of length of customers.
  By default, the provided value will be copied.  } }
}

