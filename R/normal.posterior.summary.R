#' normal posterior summary
#'
#' A function to calculate posterior quantities of bivariate normals.  See pages 74-80.
#'
#' @usage normal.posterior.summary(reps)
#' 
#' @param reps  a matrix where the columns are defined as in the output of biv.norm.post:
#'
#' @author Jeff Gill
#' @import stats
#' @seealso \code{\link{biv.norm.post}}
#' @export
normal.posterior.summary <- function(reps)  {
  reps[,5] <- reps[,5]/sqrt(reps[,3]*reps[,4])
  reps <- apply(reps,2,sort)
  out.mat <- cbind("mean"=apply(reps,2,mean), "std.err"=apply(reps,2,sd),
                   "95% HPD Lower"=reps[25,], "95% HPD Upper"=reps[975,])
  return(out.mat)
}