#' @name hanjack
#' @title hanjack
#' @description 1964 presidential election data. See page 221
#' @usage hanjack(N,F,L,W,K,IND,DEM,WR,WD,SD)
#' @param N number of cases in the group
#' @param F Observed cell proportion voting for Johnson
#' @param L log-ratio of this proportion, see p. 246
#' @param W collects the inverse of the diagonal of the matrix for the group-weighting from $[N_iP_i(1-P_i)]$
#' @param K constant
#' @param IND indifference to the election
#' @param DEM stated preference for Democratic party issues
#' @param WR Weak Republican
#' @param WD Weak Democrat
#' @param SD Strong Democrat
#' @references Hanushek, E. A. and Jackson, J. E. (1977). Statistical Methods for Social Scientists San Diego, Academic Press
#' @docType data
NULL