\name{sir}
\alias{sir}

\title{sir}
\description{
    Implementation of Rubin's Sir, see pages 338-341.
}
\usage{sir(data.mat,theta.vector,theta.mat,M,m,tol=1e-06,ll.func,df=0)}
\arguments{
    \item{data.mat}{A matrix with two columns of normally distributed data}
    \item{theta.vector}{The initial coefficient estimates}
    \item{theta.mat}{The initial vc matrix}
    \item{M}{The number of draws}
    \item{m}{The desired number of accepted  values}
    \item{tol}{The rounding/truncing tolerance}
    \item{ll.func}{loglike function for empirical posterior}
    \item{df}{ The df for using the t distribution as the approx distribution}
    
}

\keyword{models}
\examples{
	\dontrun{


sir <- function(data.mat,theta.vector,theta.mat,M,m,tol=1e-06,ll.func,df=0)  {
    importance.ratio <- rep(NA,M)
    rand.draw    <- rmultnorm(M,theta.vector,theta.mat,tol = 1e-04)
    if (df > 0) 
        rand.draw <- rand.draw/(sqrt(rchisq(M,df)/df))
    
    empirical.draw.vector <- apply(rand.draw,1,ll.func,data.mat)
    if (sum(is.na(empirical.draw.vector)) == 0)  {
	print("SIR: finished generating from posterior density function")
    	print(summary(empirical.draw.vector))
    }
    else {
	print(paste("SIR: found",sum(is.na(empirical.draw.vector)),
		"NA(s) in generating from posterior density function, quiting"))
	return()
    }

    if (df == 0)  {
        normal.draw.vector <- apply(rand.draw,1,normal.posterior.ll,data.mat)
    }
    else {
	theta.mat <- ((df-2)/(df))*theta.mat
        normal.draw.vector <- apply(rand.draw,1,t.posterior.ll,data.mat,df)
    }
    if (sum(is.na(normal.draw.vector)) == 0)  {
	print("SIR: finished generating from approximation distribution")
    	print(summary(normal.draw.vector))
    }
    else {
	print(paste("SIR: found",sum(is.na(normal.draw.vector)),
		"NA(s) in generating from approximation distribution, quiting"))
	return()
    }

    importance.ratio <- exp(empirical.draw.vector - normal.draw.vector)
    importance.ratio[is.finite=F] <- 0
    importance.ratio <- importance.ratio/max(importance.ratio)
    if (sum(is.na(importance.ratio)) == 0)  {
	print("SIR: finished calculating importance weights")
    	print(summary(importance.ratio))
    }
    else  {
	print(paste("SIR: found",sum(is.na(importance.ratio)),
		"NA(s) in calculating importance weights, quiting"))
	return()
    }

    accepted.mat <- rand.draw[1:2,]
    while(nrow(accepted.mat) < m+2)  {
	rand.unif <- runif(length(importance.ratio))
	accepted.loc <- seq(along=importance.ratio)[(rand.unif-tol) <= importance.ratio]
	rejected.loc <- seq(along=importance.ratio)[(rand.unif-tol) > importance.ratio]
	accepted.mat <- rbind(accepted.mat,rand.draw[accepted.loc,])
	rand.draw <- rand.draw[rejected.loc,]
	importance.ratio <- importance.ratio[rejected.loc]
	print(paste("SIR: cycle complete,",(nrow(accepted.mat)-2),"now accepted"))
    }
    accepted.mat[3:nrow(accepted.mat),]
}

# The following are log likelihood functions that can be plugged into the sir
function above.

logit.posterior.ll <- function(theta.vector,X)  {
    Y <- X[,1]
    X[,1] <- rep(1,nrow(X))
    sum( -log(1+exp(-X%*%theta.vector))*Y
         -log(1+exp(X%*%theta.vector))*(1-Y) )
}

normal.posterior.ll <- function(coef.vector,X)  {
    dimnames(coef.vector) <- NULL
    Y <- X[,1]
    X[,1] <- rep(1,nrow(X))
    e <- Y - X%*%solve(t(X)%*%X)%*%t(X)%*%Y
    sigma <- var(e)
    return(-nrow(X)*(1/2)*log(2*pi)
           -nrow(X)*(1/2)*log(sigma)
           -(1/(2*sigma))*(t(Y-X%*%coef.vector)%*%
                            (Y-X%*%coef.vector)) )
}

t.posterior.ll <- function(coef.vector,X,df)  {
    Y <- X[,1]
    X[,1] <- rep(1,nrow(X))
    e <- Y - X%*%solve(t(X)%*%X)%*%t(X)%*%Y
    sigma <- var(e)*(df-2)/(df)
    d <- length(coef.vector)
    return(log(gamma((df+d)/2)) - log(gamma(df/2))
                                - (d/2)*log(df)
        -(d/2)*log(pi) - 0.5*(log(sigma))
        -((df+d)/2*sigma)*log(1+(1/df)*
                          (t(Y-X%*%coef.vector)%*%
                            (Y-X%*%coef.vector)) ))
}



probit.posterior.ll <- function (theta.vector,X,tol = 1e-05)  {
    Y <- X[,1]
    X[,1] <- rep(1,nrow(X))
    Xb <- X%*%theta.vector
    h <- pnorm(Xb)
    h[h<tol] <- tol
    g <- 1-pnorm(Xb)
    g[g<tol] <- tol
    sum( log(h)*Y + log(g)*(1-Y) )
}


	}
}



