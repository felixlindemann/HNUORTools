\docType{data}
\name{bordersgermany}
\alias{bordersgermany}
\alias{bordersgermany.labels}
\alias{bordersgermany.polygon}
\alias{cities}
\title{Rudimental GIS-Data of Germany}
\format{three items as described above.}
\description{
A dataset containing the Polygons and Cities of Germany.
The variables are as follows:

\itemize{ \item cities - A data.frame with the coordinates
and names of the 57 major cities.  \item
bordersgermany.labels - The names of the regions in Germany
\item bordersgermany.polygon - A list containing the data
of the regions of Germany.  As some regions have islands
the first level again is a list containg the matrices with
the coordinates of the polygons.
\code{class(bordersgermany.polygon[[1]])} will result
"list".  \code{class(bordersgermany.polygon[[1]][[1]]))}
will result "matrix". }
}
\note{
for citing use: Felix Lindemann (2014). HNUORTools:
Operations Research Tools. R package version 1.1-0.
\url{http://felixlindemann.github.io/HNUORTools/}.
}
\section{NOTE}{
  \itemize{ \item The coordinate are assumed to be on a
  kilometer level which means, distances measured in KM can
  be calculated using euklidian distances.  \item For
  convenience the coordinates have been moved to an origin
  located on the south-east corner.  \item The accuracy of
  the data is not proven and supposed as a not bad
  approach. If serious calculations regarding real
  distances are supposed to be made, this data definitely
  is unappropriate. }
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
\keyword{datasets}

