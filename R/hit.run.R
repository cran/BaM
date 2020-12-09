#' hit.run
#'
#' Implementation of hit.run algorithm, p. 361.
#' 
#' @usage hit.run(theta.mat,reps,I.mat)
#' 
#' @param theta.mat theta.mat
#' @param reps  reps
#' @param I.mat I.mat
#'
#' @author Jeff Gill
#' @importFrom stats runif rgamma
#' @examples
#' ## Not run: 
#' #code to implement graph on p. 362, see page 376.
#' 
#' num.sims <- 10000
#' Sig.mat <- matrix(c(1.0,0.95,0.95,1.0),2,2)
#' walks<-rbind(c(-3,-3),matrix(NA,nrow=(num.sims-1),ncol=2))
#' walks <- hit.run(walks,num.sims,Sig.mat)
#' z.grid <- outer(seq(-3,3,length=100),seq(-3,3,length=100),
#'                 FUN=dmultinorm,c(0,0),Sig.mat)
#' contour(seq(-3,3,length=100),seq(-3,3,length=100),z.grid,
#'         levels=c(0.05,0.1,0.2))
#' points(walks[5001:num.sims,],pch=".")
## End(Not run)
#' @export
hit.run <- function(theta.mat,reps,I.mat)  {
  for (i in 2:reps)  {
    u.vec <- c(runif(1,0,pi/2),runif(1,pi/2,pi),
               runif(1,pi,3*pi/2),
               runif(1,3*pi/2,2*pi))
    u.dr <- sample(u.vec,size=1,
                   prob=c(1/3,1/6,1/3,1/6))
    g.ds <- rgamma(1,1,1)  
    xy.theta <- c(g.ds*cos(u.dr),g.ds*sin(u.dr)) 
    + theta.mat[(i-1),]
    a <- dmultinorm(xy.theta[1],xy.theta[2],
                    c(0,0),I.mat)/
      dmultinorm(theta.mat[(i-1),1],
                 theta.mat[(i-1),2],c(0,0),I.mat)
    r.uniform <- runif(1)
    if (a > r.uniform) theta.mat[i,] <- xy.theta
    else theta.mat[i,] <- theta.mat[(i-1),]
  }
  theta.mat
}