\docType{class}
\name{GeoSituation}
\alias{GeoSituation}
\alias{GeoSituation-class}
\title{The GeoSituation class}
\description{
This class is part of the \pkg{HNUORTools}. It can be
understood as a scenario-placeholder in an
\dfn{Operations-Research (OR)}-context.
}
\details{
Find here the defined slots for this class.
}
\note{
for citing use: Felix Lindemann (2014). HNUORTools:
Operations Research Tools. R package version 1.1-0.
\url{http://felixlindemann.github.io/HNUORTools/}.
}
\section{Slots defined}{
  \describe{ \item{\code{id}:}{Object of class
  \code{"character"}, The Id-of the Scenario.}
  \item{\code{label}:}{Object of class \code{"character"},
  The label-of the Scenario.} \item{\code{nodes}:}{Objects
  of class \code{\link{Node}} used in the Scenario.}
  \item{\code{links}:}{Objects of class \code{\link{Link}}
  used in the Scenario.} \item{\code{warehouses}:}{Objects
  of class \code{\link{Warehouse}} used in the Scenario.}
  \item{\code{customers}:}{Objects of class
  \code{\link{Customer}} used in the Scenario.}
  \item{\code{spp}:}{Object of class \code{\link{list}}.
  The items in the list are: \describe{
  \item{\code{iteration}:}{an \code{\link{numeric}} value
  indicating after which iteration the algorithm
  terminated.} \item{\code{Q}:}{A \code{\link{data.frame}}
  (sorted list) with the reachable nodes.}
  \item{\code{tableau}:}{A nx2 \code{\link{matrix}} with
  the current solution. In Column "p" is the Index(!) of
  the predecessor (not the id) stored.}
  \item{\code{finaltableau}:}{A \code{\link{matrix}} with
  the results of every iteration. In Column "p" is the
  Index(!) of the predecessor (not the id) stored.} } }
  \item{\code{tpp}:}{Object of class \code{\link{list}}.
  The items in the list are: \describe{ \item{x}{An object
  of type \code{\link{matrix}}, representing the
  Transportation Plan.} \item{cij}{An object of type
  \code{\link{matrix}}, representing the Transportation
  Costs.} \item{costfactor}{An object of type
  \code{\link{matrix}}, representing the Transportation
  Cost factor.} } } }
}

\section{Creating objects of type \code{\link{GeoSituation}}}{
  \describe{ \item{Creating an \code{S4-Object}}{
  \code{new("GeoSituation", ...)} } \item{Using the
  user-friendly function}{ \code{new("GeoSituation",...)} }
  }
}

\section{Methods}{
  \describe{ \item{\code{\link{plot}(x=GeoSituation,
  y=NULL, ...)}}{ Creates a plot the current Situation.  }
  }
}
\examples{
# \\code{citation("HNUORTools"}
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
\code{\link{Node}}, \code{\link{Link}},
\code{\link{Warehouse}}, \code{\link{Customer}}
}

