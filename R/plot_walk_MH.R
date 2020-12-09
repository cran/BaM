#' plot_walk_MH
#'
#' plot_walk_MH code used to produce figure 10.4
#' 
#' @usage plot_walk_MH(walk.mat)
#' 
#' @param walk.mat walk.mat
#'
#' @author Jeff Gill
#' @importFrom graphics plot segments
#' @export
plot_walk_MH <- function(walk.mat)  {
  plot(walk.mat[1,1],walk.mat[1,2],type="n",
       xlim=round(range(walk.mat[,1])*1.2),
       ylim=round(range(walk.mat[,2])*1.2),
       xlab="",ylab="")
  for(i in 1:(nrow(walk.mat)-1))  {
    segments(walk.mat[i,1],walk.mat[i,2],
             walk.mat[(i+1),1],walk.mat[(i+1),2])
  }
}