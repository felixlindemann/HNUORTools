\docType{class}
\name{Link}
\alias{Link}
\alias{Link-class}
\title{The Link class}
\description{
This class is part of the \pkg{HNUORTools}. It represents
the connection of two \code{\link{Node}s} in an
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
  \describe{ \item{\code{id}:}{ Object of class
  \code{"character"}, containing data from id.
  \strong{Should be unique}.  The default value will be
  caluclated randomly.  } \item{\code{label}:}{ Object of
  class \code{"character"}, containing the label of the
  \code{\link{Node}}.  The default value will be caluclated
  randomly.  } \item{\code{origin}:}{ Object of class
  \code{"Node"}, containing the origin \code{\link{Link}}.
  This value is REQUIRED.  } \item{\code{destination}:}{
  Object of class \code{"Node"}, containing the destination
  \code{\link{Link}}. This value is REQUIRED.  }
  \item{\code{costs}:}{ Object of class \code{"numeric"},
  containing the costs for using this \code{\link{Link}}.
  The default value will be caluclated automatically (if
  not provided during creation).  }
  \item{\code{distance}:}{ Object of class
  \code{"numeric"}, containing the distance for using this
  \code{\link{Link}}.  The default value will be caluclated
  automatically (if not provided during creation).  }
  \item{\code{oneway}:}{ Object of class \code{"logical"},
  indicating if this \code{\link{Link}} can be accessed
  both directions.  The default value will is \code{FALSE}.
  } \item{\code{used}:}{ Object of class \code{"logical"},
  Usually automatically assigned when solving the
  \dfn{Shortest-Path-Problem (SPP)} with the
  \code{\link{SPP.Dijkstra}}.  The default value will is
  \code{FALSE}.  } }
}

\section{Methods}{
  \describe{ \item{\code{$}}{ getting the value of a slot }
  \item{\code{$<-}}{ assinging a value to of the slots } }
}

\section{To be used for}{
  \describe{ \item{Travelling-Salesman-Problem}{ NOT YET
  IMPLEMENTED!  Finding the shortest roundtrip with e.g.
  Nearest-Neighbor-Method
  (\code{\link{TSP.NearestNeighbor}}), Two-Opt
  (\code{\link{TSP.2OPT}}), Three-Opt
  (\code{\link{TSP.3OPT}}) } \item{Shortest-Path-Problem}{
  Calculating the shortest Path in a network of
  \code{\link{Link}}s using e.g.  the algorithm of Dijkstra
  (\code{\link{SPP.Dijkstra}}), } }
}
\examples{
# demo(HNULink01)
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
\code{\link{Node}},\code{\link{SPP.Dijkstra}}
}

