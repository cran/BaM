\name{norm.known.var}
\alias{norm.known.var}
\alias{n.post1}

\title{norm.known.var}
\description{
    A function to calculate posterior quanties for a normal-normal model with known variance (pages 74-77).
    	It produces the posterior mean, variance, and 95\% credible interval for user-specified prior. 
}
\usage{n.post1(data.vec,pop.var,prior.mean,prior.var)}
\arguments{
    \item{data.vec}{a vector of assumed normally distributed data}
    \item{pop.var}{known population variance}
    \item{prior.mean}{mean of specified prior distribution for mu}
    \item{prior.var}{variance of specified prior distribution for mu}
}
\keyword{datasets}

