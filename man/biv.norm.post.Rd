\name{biv.norm.post}
\alias{biv.norm.post}
\alias{N}
\alias{dur}
\alias{rmultinorm}

\title{biv.norm.post}
\description{
   A function to calculate posterior quantities of the bivariate normal.  See pages 79-86
}
\usage{
	biv.norm.post(data.mat,alpha,beta,m,n0=5) 

}
\arguments{
    \item{data.mat}{A matrix with two columns of normally distributed data}
    \item{alpha}{Wishart first (scalar) parameter}
    \item{beta}{Wishart second (matrix) parameter}
    \item{m}{prior mean for mu}
    \item{n0}{prior confidence parameter}
}
    \examples{
\dontrun{	    
{
data.n10 <- rmultinorm(10, c(1,3), matrix(c(1.0,0.7,0.7,3.0),2,2))
rep.mat <- NULL; reps <- 1000
for (i in 1:reps)
rep.mat <- rbind(rep.mat, biv.norm.post(data.n10,3, matrix(c(10,5,5,10),2,2),c(2,2)))
round(normal.posterior.summary(rep.mat),3)

rwishart <- function(df, p = nrow(SqrtSigma), SqrtSigma = diag(p))  { 
    if((Ident <- missing(SqrtSigma)) && missing(p)) stop("either p or SqrtSigma must be specified") 
    Z <- matrix(0, p, p) 
    diag(Z) <- sqrt(rchisq(p, df:(df-p+1))) 
    if(p > 1) { 
        pseq <- 1:(p-1) 
        Z[rep(p*pseq, pseq) + unlist(lapply(pseq, seq))] <- rnorm(p*(p-1)/2) 
    } 
    if(Ident) crossprod(Z) 
    else crossprod(Z %*% SqrtSigma) 

}
if(Ident) crossprod(Z) 
else crossprod(Z) %*% SqrtSigma) 
if(Ident) crossprod(Z) 
else crossprod(Z) %*% SqrtSigma)  
}
}  
}
    
   
\keyword{models}

