% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bcp.R
\name{bcp}
\alias{bcp}
\title{bcp}
\usage{
bcp(theta.matrix, y, a, b, g, d)
}
\arguments{
\item{theta.matrix}{theta.matrix}

\item{y}{Counts of Coal Mining Disasters}

\item{a}{Alpha Value in the lambda Prior}

\item{b}{Beta Value in the lambda Prior}

\item{g}{Gamma Value in the phi Prior}

\item{d}{Delta Value in the phi Prior}
}
\description{
Implementation of bcp function, see pages 362-363 (2nd Edition).
}
\examples{
\dontrun{
bcp(theta.matrix,y,a,b,g,d)
}
}
\author{
Jeff Gill
}
