\docType{class}
\name{Customer}
\alias{Customer}
\alias{Customer-class}
\title{The Customer class}
\description{
This class is part of the \pkg{HNUORTools}. It extends the
\code{\link{Node}} with attributes used for describing
customer in an \dfn{Operations-Research (OR)}-context.
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
  \describe{ \item{\code{demand}:}{ Object of class
  \code{"numeric"}, only non-negative Values are allowed.

  \strong{The default value is} a random value using
  \code{as.numeric(sample(1:1000,1))}.  }
  \item{\code{isDummy}:}{ Object of class \code{"logical"}.
  Indicates if this is an algorithmic-created
  \code{\link{Customer}}.

  \strong{The default value is} \code{FALSE}.

  Some problems (e.g. the \dfn{Transportation-Problem
  (TPP)} can be solved only, if the sum of demand equals
  the sum of supply.  For this reason, some algorithms
  create virtual \code{\link{Customer}s} or
  \code{\link{Warehouse}s}.  } }
}

\section{Slots defined in parent class (\code{\link{Node}})}{
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

\section{Creating objects of type \code{\link{Customer}}}{
  \describe{ \item{Creating an \code{S4-Object}}{
  \code{new("Customer", ...)} } \item{Converting from a
  \code{data.frame}}{ \code{as.Customer{<data.frame>}} See
  also below in the Methods-Section.  } \item{Converting
  from a \code{list}}{ \code{as.Customer{<list>}} See also
  below in the Methods-Section.  } }
}

\section{Methods}{
  \describe{ \item{\code{as.list(obj, ...)}}{ Converts a
  \code{\link{Customer}} into a \code{\link{list}}.
  \code{...} are user-defined (not used) parameters.  }
  \item{\code{as.data.frame(obj, ...)}}{ Converts a
  \code{\link{Customer}} into a \code{\link{data.frame}}.
  \code{as.data.frame} accepts the optional parameter
  \code{withrownames} of class \code{"logical"} (default is
  \code{TRUE}).  If \code{withrownames == TRUE} the
  returned \code{\link{data.frame}} will recieve the
  \code{id} as rowname.  \code{...} are user-defined (not
  used) parameters.

  } \item{\code{as.Customer(obj)}}{ Converts an object of
  class \code{\link{data.frame}} or of class
  \code{\link{list}} into a \code{\link{Customer}}.  }
  \item{\code{is.Customer(obj)}}{ Checks if the object
  \code{obj} is of type \code{\link{Customer}}.  }
  \item{\code{\link{calc.Distance}}}{ Calculating the
  distance between two \code{\link{Node}s}.  }
  \item{\code{\link{calc.polar}}}{ Calculating the angle to
  the x-Axis of a link, connecting two \code{\link{Node}s}.
  } }
}

\section{Derived from Classes}{
  \describe{ \item{\code{\link{Node}}}{ This class extends
  the \code{\link{Node}}-Class with attributes for
  according to customers for OR-Problems.  } }
}

\section{To be used for}{
  \describe{ \item{Transportation-Problem (TPP)}{ Finding a
  transportation-plan with e.g.  North-West-Corner Rule
  (\code{\link{TPP.NW}}), Column-Minimum-Method
  (\code{\link{TPP.CMM}}), Matrix-Minimum-Method
  (\code{\link{TPP.MMM}}), .  Stepping-Stone-Method
  (\code{\link{TPP.SteppingStone}}) or MODI-Method
  (\code{\link{TPP.MODI}}).  }
  \item{Warehouse-Location-Problem (WLP)}{ using the
  ADD-Algorithm (\code{\link{WLP.ADD}}).  }
  \item{Vehicle-Routing-Problem (VRP)}{ using the
  Sweep-Algorithm (\code{\link{VRP.SWEEP}}) or
  Savings-Algorithm (\code{\link{VRP.SAVINGS}}).  } }
}
\examples{
# demo(HNUNode01)
# demo(HNUNode02)
# demo(HNUNode03)
# Creating objects of class Customer:
x <- new("Customer")

# Doing it the proper way:
x <- new("Customer", x=2, y=3, id="c1", label="customer1", demand=10)

# Deriving from a data.frame
df<- data.frame(x=2, y=3, id="c1", label="customer1", demand=10)
x <- new("Customer", df)

# Using a user-friendly-function:
# x <- new("Customer",x=2, y=3, id="c1", ...)
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
\code{\link{VRP.SAVINGS}},\code{\link{calc.Distance}},
\code{\link{Node}}, \code{\link{Warehouse}}
}

