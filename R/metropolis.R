#' metropolis
#'
#' Implementation of metropolis function, p. 359.
#' 
#' @usage metropolis(theta.matrix,reps,I.mat)
#' 
#' @param theta.matrix  theta.matrix
#' @param reps  reps
#' @param I.mat I.mat
#'
#' @author Jeff Gill
#' @import stats
#' @import MASS
#' @export
metropolis <- function(theta.matrix,reps,I.mat)  {
  for (i in 2:reps)  {
    theta.star <- mvrnorm(1,theta.matrix[(i-1),],I.mat)/
      (sqrt(rchisq(2,5)/5))
    a <-dmultinorm(theta.star[1],theta.star[2],c(0,0),I.mat)/
      dmultinorm(theta.matrix[(i-1),1],theta.matrix[(i-1),2],
                 c(0,0),I.mat)
    if (a > runif(1)) theta.matrix[i,] <- theta.star
    else theta.matrix[i,] <- theta.matrix[(i-1),]
  }     
  theta.matrix
}