\docType{methods}
\name{drawTSP}
\alias{drawTSP}
\alias{drawTSP,GeoSituation}
\alias{drawTSP,GeoSituation-method}
\title{drawTSP}
\usage{
drawTSP(object, ...)

\S4method{drawTSP}{GeoSituation}(object, ...)
}
\arguments{
  \item{object}{the \code{\link{GeoSituation}-Object(s)} to
  be plotted}

  \item{...}{list of optional values (see below)}
}
\description{
drawTSP
}
\details{
Optional Parameters for this Methdod.
}
\section{optional arguments}{
  \describe{ \item{startnode.bg.col}{\emph{Optional
  Parameter} Defines the background-color of the
  start-node. Default value is 2.}
  \item{startnode.bg.pch}{\emph{Optional Parameter} Defines
  the background-point-Style of the start-node. Default
  value is 20.} \item{startnode.bg.cex}{\emph{Optional
  Parameter} Defines the background-size of the start-node.
  Default value is 3.}
  \item{startnode.fg.col}{\emph{Optional Parameter} Defines
  the foreground-color of the start-node. Default value is
  1.} \item{startnode.fg.pch}{\emph{Optional Parameter}
  Defines the foreground-point-Style of the start-node.
  Default value is 20.}
  \item{startnode.fg.cex}{\emph{Optional Parameter} Defines
  the foreground-size of the start-node. Default value is
  1.} }
}

\section{arguments are forwarded to the following functions}{
  \describe{ \item{\code{arrows}:}{ the routing will be
  drawn using this functions. attributes
  \code{x1},\code{y1} are blocked } }
}

