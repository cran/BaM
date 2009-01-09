\name{iq}
\alias{iq}
\docType{data}
\title{iq data frame}
\description{
   	IQ data for 80 countries. See pages 93-95
}
\usage{data(iq)}


\examples{
\dontrun{
{
n <- length(iq[,1])
t.iq <- (iq[,1]-mean(iq[,1]))/(sd(iq[,1])/sqrt(n))
r.t <- (rt(100000, n-1)*(sd(iq)/sqrt(n))) + mean(iq)
quantile(r.t,c(0.01,0.10,0.25,0.5,0.75,0.90,0.99)) 
r.sigma.sq <- 1/rgamma(100000,shape=(n-2)/2, rate=var(iq[,1])*(n-1)/2)
quantile(sqrt(r.sigma.sq), c(0.01,0.10,0.25,0.5,0.75,0.90,0.99)) 
}
}
}
 
 \source{	Lynn, R. and Vanhanen, T.  (2001).  National IQ and Economic Development.
               \emph{Mankind Quarterly} LXI, 415-437.
} 
\keyword{datasets}


