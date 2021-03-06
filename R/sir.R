#' sir
#'
#' Implementation of Rubin's SIR, see pages 338-341 (2nd Edition)
#' 
#' @usage sir(data.mat,theta.vector,theta.mat,M,m,tol=1e-06,ll.func,df=0)
#' 
#' @param data.mat  A matrix with two columns of normally distributed data
#' @param theta.vector The initial coefficient estimates
#' @param theta.mat  The initial vc matrix
#' @param M The number of draws
#' @param m The desired number of accepted values
#' @param tol The rounding/truncing tolerance
#' @param ll.func loglike function for empirical posterior
#' @param df The df for using the t distribution as the approx distribution
#'
#' @author Jeff Gill
#' @importFrom stats pnorm rchisq runif
#' @aliases logit.posterior.ll normal.posterior.ll t_posterior.ll probit.posterior.ll
#' @examples
#' \dontrun{ 
#' sir <- function(data.mat,theta.vector,theta.mat,M,m,tol=1e-06,ll.func,df=0) {
#'  importance.ratio <- rep(NA,M)
#'  rand.draw <- rmultinorm(M,theta.vector,theta.mat,tol = 1e-04)
#'  if (df > 0)
#'    rand.draw <- rand.draw/(sqrt(rchisq(M,df)/df))
#'  empirical.draw.vector <- apply(rand.draw,1,ll.func,data.mat)
#'  if (sum(is.na(empirical.draw.vector)) == 0) {
#'    print("SIR: finished generating from posterior density function")
#'    print(summary(empirical.draw.vector))
#'  }
#'  else {
#'    print(paste("SIR: found",sum(is.na(empirical.draw.vector)),
#'                "NA(s) in generating from posterior density function, quiting"))
#'    return()
#'  }
#'  if (df == 0) {
#'    normal.draw.vector <- apply(rand.draw,1,normal.posterior.ll,data.mat)
#'  }
#'  else {
#'    theta.mat <- ((df-2)/(df))*theta.mat
#'    normal.draw.vector <- apply(rand.draw,1,t.posterior.ll,data.mat,df)
#'  }
#'  if (sum(is.na(normal.draw.vector)) == 0) {
#'    print("SIR: finished generating from approximation distribution")
#'    print(summary(normal.draw.vector))
#'  }
#'  else {
#'    print(paste("SIR: found",sum(is.na(normal.draw.vector)),
#'                "NA(s) in generating from approximation distribution, quiting"))
#'    return()
#'  }
#'  importance.ratio <- exp(empirical.draw.vector - normal.draw.vector)
#'  importance.ratio[is.finite=F] <- 0
#'  importance.ratio <- importance.ratio/max(importance.ratio)
#' if (sum(is.na(importance.ratio)) == 0) {
#'  print("SIR: finished calculating importance weights")
#'  print(summary(importance.ratio))
#' }
#' else {
#'   print(paste("SIR: found",sum(is.na(importance.ratio)),
#'               "NA(s) in calculating importance weights, quiting"))
#'   return()
#' }
#'  accepted.mat <- rand.draw[1:2,]
#' while(nrow(accepted.mat) < m+2) {
#'   rand.unif <- runif(length(importance.ratio))
#'   accepted.loc <- seq(along=importance.ratio)[(rand.unif-tol) <= importance.ratio]
#'   rejected.loc <- seq(along=importance.ratio)[(rand.unif-tol) > importance.ratio]
#'   accepted.mat <- rbind(accepted.mat,rand.draw[accepted.loc,])
#'   rand.draw <- rand.draw[rejected.loc,]
#'   importance.ratio <- importance.ratio[rejected.loc]
#'   print(paste("SIR: cycle complete,",(nrow(accepted.mat)-2),"now accepted"))
#' }
#' accepted.mat[3:nrow(accepted.mat),]
#' }
#' # The following are log likelihood functions that can be plugged into the sir function above.
#' 
#' logit.posterior.ll <- function(theta.vector,X) {
#'   Y <- X[,1]
#'   X[,1] <- rep(1,nrow(X))
#'   sum( -log(1+exp(-X
#'                   -log(1+exp(X)))))
#' }
#'
#' normal.posterior.ll <- function(coef.vector,X) {
#'   dimnames(coef.vector) <- NULL
#'   Y <- X[,1]
#'   X[,1] <- rep(1,nrow(X))
#'   e <- Y - X
#'   sigma <- var(e)
#'   return(-nrow(X)*(1/2)*log(2*pi)
#'          -nrow(X)*(1/2)*log(sigma)
#'          -(1/(2*sigma))*(t(Y-X)*(Y-X)))
#' }
#'
#' t.posterior.ll <- function(coef.vector,X,df) {
#'   Y <- X[,1]
#'   X[,1] <- rep(1,nrow(X))
#'   e <- Y - X
#'   sigma <- var(e)*(df-2)/(df)
#'   d <- length(coef.vector)
#'  return(log(gamma((df+d)/2)) - log(gamma(df/2))
#'        - (d/2)*log(df)
#'        -(d/2)*log(pi) - 0.5*(log(sigma))
#'        -((df+d)/2*sigma)*log(1+(1/df)*
#'                                (t(Y-X*(Y-X)))))
#' }
#'
#' probit.posterior.ll <- function (theta.vector,X,tol = 1e-05) {
#'   Y <- X[,1]
#'   X[,1] <- rep(1,nrow(X))
#'   Xb <- X
#'   h <- pnorm(Xb)
#'   h[h<tol] <- tol
#'   g <- 1-pnorm(Xb)
#'   g[g<tol] <- tol
#'   sum( log(h)*Y + log(g)*(1-Y) )
#' }
#' }
#'
#' @export
sir <- function(data.mat,theta.vector,theta.mat,M,m,tol = 1e-06,ll.func,df = 0) {
  importance.ratio <- rep(NA,M)
  rand.draw <- rmultinorm(M,theta.vector,theta.mat,tol = 1e-04)
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
    normal.draw.vector <- apply(rand.draw,1,t_posterior.ll,data.mat,df)
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
#' @export
probit.posterior.ll <- function (theta.vector,X,tol = 1e-05) {
  Y <- X[,1]
  X[,1] <- rep(1,nrow(X))
  Xb <- X
  h <- pnorm(Xb)
  h[h<tol] <- tol
  g <- 1-pnorm(Xb)
  g[g<tol] <- tol
  sum( log(h)*Y + log(g)*(1-Y) )
}
#' @export
t_posterior.ll <- function(coef.vector,X,df) {
  Y <- X[,1]
  X[,1] <- rep(1,nrow(X))
  e <- Y - X
  sigma <- var(e)*(df-2)/(df)
  d <- length(coef.vector)
  return(log(gamma((df+d)/2)) - log(gamma(df/2))
         - (d/2)*log(df)
         -(d/2)*log(pi) - 0.5*(log(sigma))
         -((df+d)/2*sigma)*log(1+(1/df)*
                                 (t(Y-X*(Y-X)))))
}
#' @export
normal.posterior.ll <- function(coef.vector,X) {
  dimnames(coef.vector) <- NULL
  Y <- X[,1]
  X[,1] <- rep(1,nrow(X))
  e <- Y - X
  sigma <- var(e)
  return(-nrow(X)*(1/2)*log(2*pi)
         -nrow(X)*(1/2)*log(sigma)
         -(1/(2*sigma))*(t(Y-X)*(Y-X)))
}
#' @export
logit.posterior.ll <- function(theta.vector,X) {
  Y <- X[,1]
  X[,1] <- rep(1,nrow(X))
  sum( -log(1+exp(-X
                  -log(1+exp(X)))))
}