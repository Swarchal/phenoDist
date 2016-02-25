#' Bhattacharyya distance
#' 
#' Measures the distance between two multivariate distributions
#' 
#' @param d1 matrix or numeric dataframe
#' @param d2 matrix or numeric dataframe
#' @param ... additional arguments to be passed to \code{cov} or \code{mean}
#' 
#' @return Bhattacharyya distance
#' 
#' @import fpc
#' 
#' @examples
#' bhatt_dist(iris[,1:4], iris[,1:4])
#' bhatt_dist(iris[1:10, 1:4], iris[20:30, 1:4])
#' 
#' @export


bhatt_dist <- function(d1, d2,...){
  
  m1 <- as.matrix(d1)
  m2 <- as.matrix(d2)
  cov1 <- cov(m1,...)
  cov2 <- cov(m2,...)
  mu1 <- colMeans(m1,...)
  mu2 <- colMeans(m2,...)

  bhattacharyya.dist(mu1 = mu1, mu2 = mu2, Sigma1 = cov1, Sigma2 = cov2)
  
}