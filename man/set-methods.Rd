\docType{methods}
\name{$<-,Node-method}
\alias{$<-}
\alias{$<-,Customer-method}
\alias{$<-,GeoSituation-method}
\alias{$<-,Link-method}
\alias{$<-,Node-method}
\alias{$<-,Warehouse-method}
\title{Set Method}
\usage{
\S4method{$}{Node}(x, name) <- value

\S4method{$}{Customer}(x, name) <- value

\S4method{$}{Link}(x, name) <- value

\S4method{$}{GeoSituation}(x, name) <- value

\S4method{$}{Warehouse}(x, name) <- value
}
\arguments{
  \item{x}{ object from which to extract element(s) or in
  which to replace element(s).  }

  \item{name}{ A literal character string or a \link{name}
  (possibly \link{backtick} quoted).  For extraction, this
  is normally (see under \sQuote{Environments}) partially
  matched to the \code{\link{names}} of the object.  }

  \item{value}{typically an array-like \R object of a
  similar class as \code{x}.}
}
\description{
Set Method

Set Method

Set Method

Set Method
}

