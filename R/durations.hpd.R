#' durations.hpd
#'
#' Simple HPD calculator from Chapter 2 (page 51, 2nd Edition).
#' 
#' @usage durations.hpd(support,fn.eval,start,stop,target=0.90,tol=0.01)
#' 
#' @param support x-axis values
#' @param fn.eval function values at x-axis points
#' @param start starting point in the vectors
#' @param stop stoppng point in the vectors
#' @param target Desired X Level
#' @param tol Tolerance for round-off
#'
#' @author Jeff Gill
#' @importFrom stats pgamma
#' @examples
#' \dontrun{
#'   get("cabinet.duration")
#'   ruler <- seq(0.45,0.75,length=10000)
#'   g.vals <- round(dgamma(ruler,shape=sum(cabinet.duration$N), 
#'                   rate=sum(cabinet.duration$N*cabinet.duration$dur)),2)
#'   start.point  <- 1000; stop.point <- length(g.vals)
#'   durations.hpd(ruler,g.vals,start.point,stop.point)
#' }
#' 
#' @export
durations.hpd <- function(support,fn.eval,start=1,stop=length(support),target=0.90,tol=0.01)  {
  get("cabinet.duration")
  cabinet.duration <- cabinet.duration
  done   <- FALSE; i <- start
  while (i < stop & done == FALSE)  {
    j <- length(fn.eval)/2
    while (j <= stop & done == FALSE)  {
      if (fn.eval[i] == fn.eval[j])  {
        L <- pgamma(support[i],shape=sum(cabinet.duration$N),rate=sum(cabinet.duration$N*cabinet.duration$dur))
        H <- pgamma(support[j],shape=sum(cabinet.duration$N),rate=sum(cabinet.duration$N*cabinet.duration$dur))
        if (((H-L)<(target+tol)) & ((H-L)>(target-tol)))  done <- TRUE
      }
      j <- j+1
    }
    i <- i+1
  }
  return(c(k=fn.eval[i], HPD.L=support[i], HPD.U=support[j]))
}