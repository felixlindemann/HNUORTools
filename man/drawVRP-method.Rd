\docType{methods}
\name{drawVRP}
\alias{drawVRP}
\alias{drawVRP,GeoSituation}
\alias{drawVRP,GeoSituation-method}
\title{drawVRP}
\usage{
drawVRP(object, ...)

\S4method{drawVRP}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{the \code{\link{GeoSituation}-Object(s)} to
  be plotted}

  \item{...}{list of optional values (see below)}
}
\description{
drawVRP
}
\details{
Optional Parameters for this Methdod.
}
\section{optional arguments}{
  \describe{ \item{\code{drawroutingtowarehouse}:}{
  \code{\link{logical}} default = \code{TRUE}. If
  \code{TRUE}, routes will be connected to the delivering
  \code{\link{Warehouse}}.  } \item{\code{arrow.bg.col}:}{
  \code{\link{numeric}} or \code{\link{character}} default
  = 1. A vector of length 1 or M (=number of
  \code{\link{Warehouse}s}). Colornames are accepted as
  well } }
}

\section{arguments are forwarded to the following functions}{
  \describe{ \item{\code{arrows}:}{ the routing will be
  drawn using this functions. attributes
  \code{x1},\code{y1},\code{col} are blocked } }
}

