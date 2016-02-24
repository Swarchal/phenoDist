#' Manhattan distance between two vectors
#'
#' A Manhattan or cityblock distance is the sum of the absolute difference
#' between all co-ordinates.
#'
#' @param x numeric vector
#' @param y numeric vector
#'
#' @return numeric distance metric
#'
#' @examples
#' a <- c(1,2,3,4,5,1,2,2)
#' b <- c(1,3,5,3,5,2,4,1)
#' manhattan(a, b)
#'
#' @export


manhattan <- function(x, y){
    stopifnot(length(x) == length(y))
    sum(abs(x - y))
}


#' Normalised Manhattan distance
#'
#' Manhattan distance normalised by the length of the vectors. This is the sum
#' of the absolute differnce between two vectors divided by the length of the
#' vectors. i.e \code{a <- c(1,2,3); b <- c(3,2,1) ; manhattan(a, b)/3}
#'
#' @param x numeric vector
#' @param y numeric vector
#'
#' @return numeric distance metric
#' @examples
#' a <- c(1,2,3,4,2,13,2)
#' b <- c(1,2,4,5,3,23,0)
#'
#' manhattan_norm(a, b)
#'
#' @export

manhattan_norm <- function(x, y){
    stopifnot(length(x) == length(y))
    len <- length(x)
    manhattan(x, y) / len
}
