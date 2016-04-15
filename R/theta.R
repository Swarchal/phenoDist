#' Angle between two vectors
#' 
#' Calculates the angle between two vectors, output in degrees
#' 
#' @param a Vector
#' @param b Vector
#' 
#' @return Angle in degrees
#' 
#' @export
#'
#' @examples
#' a <- c(1, 2)
#' b <- c(3, 1)
#' 
#' theta(a,b)
#' 
#' a <- c(1, 2)
#' b <- c(-1, -2)
#' theta(a, b)

theta <- function(a, b){
    
    as.vector(acos(a %*% b / (norm_vector(a) * norm_vector(b)))) * 180/pi
}
