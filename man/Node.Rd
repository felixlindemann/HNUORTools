\docType{class}
\name{Node}
\alias{Node}
\alias{Node-class}
\title{The Node class}
\description{
This class is part of the \pkg{HNUORTools}. It represents
the base class for every locateable class in an
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
\section{Slots}{
  \describe{ \item{\code{id}:}{ Object of class
  \code{"character"}, containing data from id.
  \strong{Should be unique}.  The default value will be
  caluclated randomly.  } \item{\code{label}:}{ Object of
  class \code{"character"}, containing the label of the
  \code{\link{Node}}.  The default value will be caluclated
  randomly.  } \item{\code{x}:}{ Object of class
  \code{"numeric"}, containing the x-coordinate of the
  \code{\link{Node}}.  The default value will be caluclated
  randomly.  } \item{\code{y}:}{ Object of class
  \code{"numeric"}, containing the y-coordinate of the
  \code{\link{Node}}.  The default value will be caluclated
  randomly.  } }
}

\section{Creating objects of type \code{\link{Node}}}{
  \describe{ \item{Creating an \code{S4-Object}}{
  \code{new("Node", ...)} } \item{Converting from a
  \code{data.frame}}{ \code{as.Node{<data.frame>}} See also
  below in the Methods-Section.  } \item{Converting from a
  \code{list}}{ \code{as.Node{<list>}} See also below in
  the Methods-Section.  } }
}

\section{Methods}{
  \describe{ \item{\code{as.list(node, ...)}}{ Converts a
  \code{\link{Node}} into a \code{\link{list}}.  \code{...}
  are user-defined (not used) parameters.  }
  \item{\code{as.data.frame(node, ...)}}{ Converts a
  \code{\link{Node}} into a \code{\link{data.frame}}.
  \code{as.data.frame} accepts the optional parameter
  \code{withrownames} of class \code{"logical"} (default is
  \code{TRUE}).  If \code{withrownames == TRUE} the
  returned \code{\link{data.frame}} will recieve the
  \code{id} as rowname.  \code{...} are user-defined (not
  used) parameters.

  } \item{\code{as.Node(obj)}}{ Converts an object of class
  \code{\link{data.frame}} or of class \code{\link{list}}
  into a \code{\link{Node}}.  } \item{\code{is.Node(obj)}}{
  Checks if the object \code{obj} is of type
  \code{\link{Node}}.  }
  \item{\code{\link{calc.Distance}}}{ Calculating the
  distance between two \code{\link{Node}s}.  }
  \item{\code{\link{calc.polar}}}{ Calculating the angle to
  the x-Axis of a link, connecting two \code{\link{Node}s}.
  } }
}

\section{Derived Classes}{
  \describe{ \item{\code{\link{Customer}}}{ This class
  extends the \code{\link{Node}}-Class with attributes for
  according to customers for OR-Problems.  }
  \item{\code{\link{Warehouse}}}{ This class extends the
  \code{\link{Node}}-Class with attributes for according to
  warehouses for OR-Problems.  } \item{\code{\link{Link}}}{
  This class is constructed by two entities of the
  \code{\link{Node}}-Class representing the origin and
  destination of \code{\link{Link}} for OR-Problems.  } }
}

\section{To be used for}{
  \describe{ \item{Travelling-Salesman-Problem}{ Finding
  the shortest roundtrip with e.g.  Nearest-Neighbor-Method
  (\code{\link{TSP.NearestNeighbor}}), Two-Opt
  (\code{\link{TSP.2OPT}}), Three-Opt
  (\code{\link{TSP.3OPT}}) } \item{Shortest-Path-Problem}{
  Calculating the shortest Path in a network of
  \code{\link{Link}}s using e.g.  the algorithm of Dijkstra
  (\code{\link{SPP.Dijkstra}}), } }
}
\examples{
# demo(HNUNode01)
# demo(HNUNode02)
# demo(HNUNode03)
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
The classes are derived from this class and the following
Methods can be used with this class.:
}

