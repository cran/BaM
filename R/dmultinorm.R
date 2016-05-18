#' dmultinorm
#'
#' dmultinorm function, see page 376.
#' 
#' @usage dmultinorm(xval,yval,mu.vector,sigma.matrix)
#' 
#' @param xval Vector of X Random Variables
#' @param yval Vector of Y Random Variables
#' @param mu.vector Mean Vector
#' @param sigma.matrix Matrix of Standard Deviations
#'
#' @author Jeff Gill
#' @export
dmultinorm <- function(xval,yval,mu.vector,sigma.matrix)  {
  normalizer <- (2*pi*sigma.matrix[1,1]*sigma.matrix[2,2]
                 *sqrt(1-sigma.matrix[1,2]^2))^(-1)
  like <- exp(-(1/(2*(1-sigma.matrix[1,2]^2)))* (
    ((xval-mu.vector[1])/sigma.matrix[1,1])^2
    -2*sigma.matrix[1,2]*(((xval-mu.vector[1])/sigma.matrix[1,1])*
                            ((yval-mu.vector[2])/sigma.matrix[2,2]))
    +((yval-mu.vector[2])/sigma.matrix[2,2])^2 ))
  normalizer*like
}