#' biv.norm.post
#'
#' A function to calculate posterior quantities of the bivariate normal.  See page 94.
#' 
#' @usage biv.norm.post(data.mat,alpha,beta,m,n0=5)
#' 
#' @param data.mat	A matrix with two columns of normally distributed data
#' @param alpha Wishart first (scalar) parameter
#' @param beta  Wishart second (matrix) parameter
#' @param m prior mean for mu
#' @param n0 prior confidence parameter
#'
#' @return Returns
#'  \item{mu2}{posterior mean, dimension 1}
#'  \item{sig1}{posterior mean, dimension 2}
#'  \item{sig2}{posterior variance, dimension 1}
#'  \item{rho}{posterior variance, dimension 2}
#' @author Jeff Gill
#' @import dlm
#' @import stats
#' @examples
#' 
#'  rwishart <- function(df, p = nrow(SqrtSigma), SqrtSigma = diag(p))  { 
#'  if((Ident <- missing(SqrtSigma)) && missing(p)) stop("either p or SqrtSigma must be specified") 
#'  Z <- matrix(0, p, p) 
#'  diag(Z) <- sqrt(rchisq(p, df:(df-p+1))) 
#'  if(p > 1) { 
#'    pseq <- 1:(p-1) 
#'    Z[rep(p*pseq, pseq) + unlist(lapply(pseq, seq))] <- rnorm(p*(p-1)/2) 
#'  } 
#'  if(Ident) crossprod(Z) 
#'  else crossprod(Z %*% SqrtSigma)
#'  }
#'   
#'   data.n10 <- rmultinorm(10, c(1,3), matrix(c(1.0,0.7,0.7,3.0),2,2))
#'   rep.mat <- NULL; reps <- 1000
#'   for (i in 1:reps){
#'     rep.mat <- rbind(rep.mat, biv.norm.post(data.n10,3, matrix(c(10,5,5,10),2,2),c(2,2)))
#'   }
#'   round(normal.posterior.summary(rep.mat),3)
#'     
#' @export
biv.norm.post <- function(data.mat,alpha,beta,m,n0=5) {
  n <- nrow(data.mat)
  xbar <- apply(data.mat,2,mean)
  S2 <- (n-1)*var(data.mat)
  Wp.inv <- solve(beta)+S2+((n0*n)/(n0+n))*(xbar-m)%*%t(xbar-m)
  Sigma <- solve(rwishart(alpha+n,SqrtSigma=chol(solve(Wp.inv))))
  mu <- rmultinorm(1, (n0*m + n*xbar)/(n0+n), Sigma/(n0+n))
  return(c(mu1=mu[1],mu2=mu[2],sig1=Sigma[1,1], sig2=Sigma[2,2],rho=Sigma[2,1]))
}