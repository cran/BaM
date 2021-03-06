#' rmultinorm
#'
#' a function to generate random multivariate Gaussians.
#' 
#' @usage rmultinorm(n, mu, vmat, tol = 1e-07)
#' 
#' @param n nu
#' @param mu vector of mean
#' @param vmat variance-covariance matriz
#' @param tol tolerance
#' 
#' @author Jeff Gill
#' @importFrom stats rnorm
#' @seealso \code{\link{biv.norm.post}}
#' @export
rmultinorm <- function(n, mu, vmat, tol = 1e-07) {
  p <- ncol(vmat)
  if(length(mu)!=p)
    stop(paste("mu vector is the wrong
                       length:",length(mu)))
  if(max(abs(vmat - t(vmat))) > tol)
    stop("vmat not symmetric")
  vs <- svd(vmat)
  vsqrt <- t(vs$v %*% (t(vs$u) * sqrt(vs$d)))
  ans <- matrix(rnorm(n * p), nrow = n) %*% vsqrt
  ans <- sweep(ans, 2, mu, "+")
  dimnames(ans) <- list(NULL, dimnames(vmat)[[2]])
  ans
}