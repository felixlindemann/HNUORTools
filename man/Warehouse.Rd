\docType{class}
\name{Warehouse}
\alias{Warehouse}
\alias{Warehouse-class}
\title{The Warehouse class}
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
  \describe{ \item{\code{supply}:}{ Object of class
  \code{"numeric"}, containing the amount for the supply.
  The default value cannot be negative.  }
  \item{\code{fixcosts}:}{ Object of class
  \code{"numeric"}, containing the amount for the fixcosts
  The default value cannot be negative.  }
  \item{\code{open}:}{ Object of class \code{"logical"},
  indicating if a Warehouse is used within a WLP.  }
  \item{\code{vrp}:}{ Object of class \code{"list"}, a List
  of Tours genereated for a VRP.  } \item{\code{isDummy}:}{
  Object of class \code{"logical"}, indicating if a
  Warehouse was added in an algorithm (e.g.
  TPP-Column-Minimum-Method) in order to avoid degenerated
  soulutions.  } }
}

\section{Slots (from \code{\link{Node}})}{
  \describe{ \item{\code{id}:}{ Object of class
  \code{"character"}, containing data from id.
  \strong{Should be unique}.  The default value will be
  caluclated randomly.  } \item{\code{label}:}{ Object of
  class \code{"character"}, containing the label of the
  \code{\link{Warehouse}}.  The default value will be
  caluclated randomly.  } \item{\code{x}:}{ Object of class
  \code{"numeric"}, containing the x-coordinate of the
  \code{\link{Warehouse}}.  The default value will be
  caluclated randomly.  } \item{\code{y}:}{ Object of class
  \code{"numeric"}, containing the y-coordinate of the
  \code{\link{Warehouse}}.  The default value will be
  caluclated randomly.  } }
}

\section{Creating objects of type \code{\link{Warehouse}}}{
  \describe{ \item{Creating an \code{S4-Object}}{
  \code{new("Warehouse", ...)} } \item{Converting from a
  \code{\link{data.frame}}}{
  \code{as.Warehouse{<data.frame>}} See also below in the
  Methods-Section.  } \item{Converting from a
  \code{\link{list}}}{ \code{as.Warehouse{<list>}} See also
  below in the Methods-Section.  } }
}

\section{Methods}{
  \describe{ \item{\code{as.list(Warehouse, ...)}}{
  Converts a \code{\link{Warehouse}} into a
  \code{\link{list}}.  \code{...} are user-defined (not
  used) parameters.  } \item{\code{as.data.frame(Warehouse,
  ...)}}{ Converts a \code{\link{Warehouse}} into a
  \code{\link{data.frame}}.  \code{as.data.frame} accepts
  the optional parameter \code{withrownames} of class
  \code{"logical"} (default is \code{TRUE}).  If
  \code{withrownames == TRUE} the returned
  \code{\link{data.frame}} will recieve the \code{id} as
  rowname.  \code{...} are user-defined (not used)
  parameters.  } \item{\code{as.Warehouse(obj)}}{ Converts
  an object of class \code{\link{data.frame}} or of class
  \code{\link{list}} into a \code{\link{Warehouse}}.  }
  \item{\code{is.Warehouse(obj)}}{ Checks if the object
  \code{obj} is of type \code{\link{Warehouse}}.  }
  \item{\code{\link{getDistance}}}{ Calculating the
  distance between two \code{\link{Node}s} (As the classes
  \code{\link{Customer}} and \code{\link{Warehouse}} depend
  on \code{\link{Node}}, distances can be calculated
  between any of these objects).  }
  \item{\code{\link{getpolar}}}{ Calculating the
  polar-angle to the x-Axis of a link, connecting two
  \code{\link{Node}s} (As the classes
  \code{\link{Customer}} and \code{\link{Warehouse}} depend
  on \code{\link{Node}}, polar-angles can be calculated
  between any of these objects).  } }
}

\section{Derived Classes}{
  None.
}

\section{To be used for}{
  \describe{ \item{WLP}{ Warehouse-Location-Problem }
  \item{TPP}{ Transportation Problem } \item{VRP}{ Vehicle
  Routing Problem } }
}
\examples{
# create a new Warehouse with specific values
x<- new("Warehouse", x= 10, y=20, id="myid", label = "mylabel", supply = 20)
x
# create from data.frame
df<- data.frame(x=10,y=20, supply = 30)
new("Warehouse", df)
as(df, "Warehouse")
as.Warehouse(df)
#create some Warehouses
n1 <- new("Warehouse",x=10,y=20, supply = 30, id ="n1")
n2 <- new("Warehouse",x=13,y=24, supply = 40, id ="n2")
# calculate Beeline distance
#getDistance(n1,n2) # should result 5
#getDistance(n1,n2, costs = 2) # should result 10
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

