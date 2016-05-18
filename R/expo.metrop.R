#' expo.metrop
#'
#' Simple Metropolis algorithm demonstration using a bivariate exponential target from Chapter 1 (pages 27-30).
#' 
#' @usage expo.metrop(m,x,y,L1,L2,L,B)
#' 
#' @param m number of iterations
#' @param x starting point for the x vector
#' @param y starting point for the y vector
#' @param L1  event intensity for the x dimension
#' @param L2  event intensity for the y dimension
#' @param L shared event intensity
#' @param B upper bound
#'
#' @author Jeff Gill
#' @import stats
#' @examples
#' 
#' expo.metrop(m=5000, x=0.5, y=0.5, L1=0.5, L2=0.1, L=0.01, B=8)
#'
#' @export
expo.metrop <- function(m=5000, x=0.5, y=0.5, L1=0.5, L2=0.1, L=0.01, B=8)  {
  for (i in 1:(m-1))  {
    cand.val <- cand.gen(B,B)
    a <- biv.exp(cand.val[1],cand.val[2],L1,L2,L) / biv.exp(x[i],y[i],L1,L2,L) 
    if (a > runif(1)) { 
      x <- c(x,cand.val[1])
      y <- c(y,cand.val[2])
    }
    else  {
      x <- c(x,x[i])
      y <- c(y,y[i])
    }
  }
  return(cbind(x,y))
}
biv.exp <- function(x,y,L1,L2,L) exp( -(L1+L)*x - (L2+L)*y -L*max(x,y) )
cand.gen <- function(max.x,max.y) c(runif(1,0,max.x),runif(1,0,max.y))