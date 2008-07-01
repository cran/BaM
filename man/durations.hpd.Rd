\name{durations.hpd}
\alias{durations.hpd}
\alias{simple.hpd}

\title{durations.hpd}
\description{
  Simple HPD calculator from Chapter 2 (page 51).}
\usage{simple.hpd(support,fn.eval,start,stop,target=0.90,tol=0.01)} 
\arguments{
    \item{support}{x-axis values}
    \item{fn.eval}{function values at x-axis points}
    \item{start}{starting point in the vectors}
    \item{stop}{stoppng point in the vectors}
    \item{target}{Desired X Level}
    \item{tol}{Tolerance for round-off}    

}    

\keyword{datasets}
\examples{
\dontrun{
{
data(cabinet.duration)
  ruler <- seq(0.45,0.75,length=10000)
  g.vals <- round(dgamma(ruler,shape=sum(N), rate=sum(N*dur)),2)
  start.point  <- 1000; stop.point <- length(g.vals)
  simple.hpd(ruler,g.vals,start.point,stop.point)
}
}
}

