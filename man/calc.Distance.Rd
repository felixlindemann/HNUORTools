\docType{methods}
\name{calc.Distance}
\alias{calc.Distance}
\title{distance between two \code{\link{Node}s}}
\usage{
calc.Distance(n1, n2, ...)
}
\arguments{
  \item{n1}{an object of class \code{"Node"} or a class
  derived from \code{"Node"}.}

  \item{n2}{an object of class \code{"Node"} or a class
  derived from \code{"Node"}.}

  \item{...}{\emph{Optional Parameters} See Below.}
}
\description{
Calculates the distance between to nodes.
}
\section{Optional Parameters (\code{...})}{
  \subsection{used by \code{\link{calc.Distance}}}{
  \describe{ \item{log}{\code{"logical"} Optional
  Parameter. Indicating, if the calculation should be
  logged to console. Default is \code{FALSE}.}
  \item{costfactor}{\code{"numeric"} Optional Parameter.
  Used to transform the distance by a factor into costs.
  Default is \code{1}.} } } \subsection{Forwarded to the
  follwowing functions}{ You may want to check these
  functions for any other optional parameters.  \itemize{
  \item{\code{\link{...}} currently not used and not
  forewarded. } } }
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

