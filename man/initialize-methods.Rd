\docType{methods}
\name{initialize}
\alias{initialize}
\alias{initialize,Customer-method}
\alias{initialize,GeoSituation-method}
\alias{initialize,Link-method}
\alias{initialize,Node-method}
\alias{initialize,Warehouse-method}
\title{initialize Method}
\usage{
\S4method{initialize}{Node}(.Object, data = NULL, ...)

\S4method{initialize}{Customer}(.Object, data = NULL, ...)

\S4method{initialize}{Link}(.Object, n1, n2, ...)

\S4method{initialize}{Warehouse}(.Object, data = NULL, ...)

\S4method{initialize}{GeoSituation}(.Object, data = NULL, ...)
}
\arguments{
  \item{data}{can of type \code{\link{data.frame}} or
  \code{\link{list}}}

  \item{n1}{Origin-\code{\link{Node}}}

  \item{n2}{Destination-\code{\link{Node}}}

  \item{.Object}{ An object: see the Details section.}

  \item{...}{data to include in the new object.  Named
  arguments correspond to slots in the class definition.
  Unnamed arguments must be objects from classes that this
  class extends.}
}
\description{
initialize Method

initialize Method

initialize Method

initialize Method

initialize Method
}

