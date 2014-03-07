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
  The label-of the Scenario.} \item{\code{nodes}:}{Object
  of class \code{"list"}, a list of the \code{\link{Node}s}
  used in the Scenario.} \item{\code{links}:}{Object of
  class \code{"list"}, a list of the \code{\link{Link}s}
  used in the Scenario.} \item{\code{warehouses}:}{Object
  of class \code{"list"}, a list of the
  \code{\link{Warehouse}s} used in the Scenario.}
  \item{\code{customers}:}{Object of class \code{"list"}, a
  list of the \code{\link{Customer}s} used in the
  Scenario.} \item{\code{tpp.costs}:}{Object of class
  \code{"matrix"}, the cij for a Transportation-Problem in
  the Scenario.} \item{\code{tpp.costs.opp}:}{Object of
  class \code{"matrix"}, the wij (opportunity-costs) for a
  Transportation-Problem in the Scenario.}
  \item{\code{tpp.x}:}{Object of class \code{"matrix"}, the
  xij (transportation-plan) for a Transportation-Problem in
  the Scenario.} \item{\code{wlp.costs}:}{Object of class
  \code{"matrix"}, the cij for a Warehouse-Location-Problem
  in the Scenario.} \item{\code{wlp.solution}:}{Object of
  class \code{"list"}, the solution for a
  Warehouse-Location-Problem in the Scenario.}
  \item{\code{shortespath}:}{Object of class \code{"list"},
  the solution for a Shortest-Path-Problem in the
  Scenario.} \item{\code{tsp.costs}:}{Object of class
  \code{"matrix"}, the cij used in a
  Travelling-Salesman-Problem in the Scenario.}
  \item{\code{tsp.nodes}:}{Object of class \code{"list"}, a
  list of \code{\link{Node}s} used in a
  Travelling-Salesman-Problem in the Scenario.}
  \item{\code{tsp.solution}:}{Object of class
  \code{"list"}, the solution of a
  Travelling-Salesman-Problem in the Scenario.} }
}

\section{Creating objects of type \code{\link{GeoSituation}}}{
  \describe{ \item{Creating an \code{S4-Object}}{
  \code{new("GeoSituation", ...)} } \item{Using the
  user-friendly function}{ \code{new("GeoSituation",...)} }
  }
}

\section{Methods}{
  \describe{ \item{\code{\link{plotGeoSituation}}}{ Creates
  a plot the current Situation.  } }
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
\code{\link{TPP.NW}},\code{\link{TPP.CMM}},\code{\link{TPP.MMM}},
\code{\link{TPP.SteppingStone}},\code{\link{TPP.MODI}},
\code{\link{WLP.ADD}},\code{\link{VRP.SWEEP}},
\code{\link{VRP.SAVINGS}},\code{\link{getDistance}},
\code{\link{Node}}, \code{\link{Warehouse}}
}

