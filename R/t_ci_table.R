#' t_ci_table
#'
#' A function to calculate credible intervals and make a table.  See page 169.
#' 
#' @usage t_ci_table(coefs,cov.mat,level=0.95,degrees=Inf,quantiles=c(0.025,0.500,0.975)) 
#' 
#' @param coefs    	vector of coefficient estimates, usually posterior means
#' @param cov.mat		variance-covariance matrix 
#' @param level		desired coverage level
#' @param degrees		degrees of freedom parameter for students-t distribution assumption
#' @param quantiles	vector of desired CDF points (quantiles) to return
#' 
#' @return quantile.mat	matrix of quantiles
#' 
#' @author Jeff Gill
#' @import stats
#' @export
t_ci_table <- function(coefs,cov.mat,level=0.95,degrees=Inf,quantiles=c(0.025,0.500,0.975)) {
  quantile.mat <- cbind( coefs, sqrt(diag(cov.mat)),
                         t(qt(quantiles,degrees) %o% sqrt(diag(cov.mat)))
                         + matrix(rep(coefs,length(quantiles)),
                                  ncol=length(quantiles)) )
  quantile.names <- c("Mean","Std. Error")
  for (i in 1:length(quantiles))
    quantile.names <- c(quantile.names,paste(quantiles[i],
                                             "Quantile"))
  dimnames(quantile.mat)[2] <- list(quantile.names)
  return(list(title="Posterior Quantities",round(quantile.mat,4)))
}