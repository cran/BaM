#' norm.known.var
#'
#' A function to calculate posterior quanties for a normal-normal model with known variance (pages 70-72).
#' It produces the posterior mean, variance, and 95\% credible interval for user-specified prior. 
#' 
#' @usage norm.known.var(data.vec,pop.var,prior.mean,prior.var)
#' 
#' @param data.vec	a vector of assumed normally distributed data
#' @param pop.var		known population variance
#' @param prior.mean	mean of specified prior distribution for mu
#' @param prior.var 	variance of specified prior distribution for mu
#'
#' @author Jeff Gill
#'
#' @export
norm.known.var <- function(data.vec,pop.var,prior.mean,prior.var)  {
  if(length(data.vec) <= 1) stop("norm.known.var: input data must be a vector")
  mu.hat <- (prior.mean/prior.var + length(data.vec)*mean(data.vec)/pop.var)/
    (1/prior.var + length(data.vec)/pop.var)
  sigma.hat <- 1/(1/prior.var + length(data.vec)/pop.var)
  credible.int<-c(mu.hat-1.96*sqrt(sigma.hat),mu.hat+1.96*sqrt(sigma.hat))
  return( list(mu.hat=mu.hat,sigma.hat=sigma.hat, credible.interval=credible.int) )
}