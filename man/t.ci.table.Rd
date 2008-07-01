\name{t.ci}
\alias{t.ci}
\alias{t}
\title{Analysis of Cultural Consensus Data}
\description{
    Analysis of cultural consensus data using binomial likelihood and beta prior.}

\usage{
	\method{t}{ci}(coefs, cov.mat,level = 0.95, degrees = Inf,quantiles= c(0.025,0.500,0.975))

}

\arguments{
	\item{coefs}{vector of coefficient estimates, usually posterior means}
	\item{cov.mat}{variance-covariance matrix}
	\item{level}{desired coverage level}
	\item{degrees}{degrees of freedom parameter for students-t distribution assumption}
	\item{quantiles}{vcector of desired CDF points(quantiles) to return}
	
}
 
\keyword{datasets}

\examples{
	\dontrun{

# Examples on pages 225-226. 
# READ IN THE DATA AND USE MULTIPLE IMPUTATION ON MISSING
data(pbc.vote)
X <- cbind(tech, new, turnout, rep, whi)
Y <- badballots
library(nnet); library(mice)
imp.X <- mice(X)
X <- as.matrix(cbind(rep(1,nrow(X)), complete(imp.X)))

# UNINFORMED PRIOR ANALYSIS
bhat <- solve(t(X)%*%X)%*%t(X)%*%Y
s2 <- t(Y- X%*%bhat)%*%(Y- X%*%bhat)/(nrow(X)-ncol(X))
R <- solve(t(X)%*%X)*((nrow(X)-ncol(X))*
        s2/(nrow(X)-ncol(X)-2))[1,1]
uninformed.table <- t.ci.table(bhat,R,
        degrees=nrow(X)-ncol(X))[[2]]
alpha <- (nrow(X)-ncol(X)-1)/2
beta <- 0.5*s2*(nrow(X)-ncol(X))
sort.inv.gamma.sample <- sort(1/rgamma(10000,alpha,beta))
sqrt.sort.inv.gamma.sample <- sqrt(sort.inv.gamma.sample)
uninformed.table <- rbind(uninformed.table,
       c( mean(sqrt.sort.inv.gamma.sample),
      sqrt(var(sqrt.sort.inv.gamma.sample)),
               sqrt.sort.inv.gamma.sample[250],
               sqrt.sort.inv.gamma.sample[5000],
               sqrt.sort.inv.gamma.sample[9750] ))

# CONJUGATE PRIOR ANALYSIS
A <- 3; B <- 9
BBeta <- rep(0,6); Sigma <- diag(c(2,2,2,2,2,2))
tB <- solve(solve(Sigma)
    + t(X)%*%X)%*%(solve(Sigma)%*%BBeta+t(X)%*%X%*%bhat)
ts <- 2*B + s2*(nrow(X)-ncol(X)) + (t(BBeta)-t(tB))%*%
     solve(Sigma)%*%BBeta + t(bhat-tB)%*%t(X)%*%X%*%bhat
R <- diag(ts/(nrow(X)+A-ncol(X)-2))*
     solve(solve(Sigma)+t(X)%*%X)
conjugate.table<-t.ci.table(tB,R,
    degrees=nrow(X)+A-ncol(X)-3)[[2]]
sort.inv.gamma.sample <- sort(1/rgamma(10000,alpha,beta))
sqrt.sort.inv.gamma.sample <- sqrt(sort.inv.gamma.sample)
conjugate.table<- rbind(conjugate.table,
    c( mean(sqrt.sort.inv.gamma.sample),
   sqrt(var(sqrt.sort.inv.gamma.sample)),
            sqrt.sort.inv.gamma.sample[250],
            sqrt.sort.inv.gamma.sample[5000],
            sqrt.sort.inv.gamma.sample[9750] ))


	}
}
