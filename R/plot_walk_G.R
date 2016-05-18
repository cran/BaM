#' plot_walk_G
#'
#' plot_walk_G code used to produce figure 10.2
#' 
#' @usage plot_walk_G(walk.mat,sim.rm,X=1,Y=2)
#' 
#' @param walk.mat walk.mat
#' @param sim.rm sim.rm
#' @param X X
#' @param Y Y
#'
#' @author Jeff Gill
#' @import graphics
#' @export
plot_walk_G <- function(walk.mat,sim.rm,X=1,Y=2) {
  plot(walk.mat[1,X],walk.mat[1,Y],type="n",
       xlim=range(walk.mat[,X]),
       ylim=range(walk.mat[,Y]),
       xlab="",ylab="")
  for(i in 1:(nrow(walk.mat)-1))  {
    segments(walk.mat[i,X],walk.mat[i,Y],
             walk.mat[(i+1),X],walk.mat[i,Y])
    segments(walk.mat[(i+1),X],walk.mat[i,Y],
             walk.mat[(i+1),X],walk.mat[(i+1),Y])
  }
}