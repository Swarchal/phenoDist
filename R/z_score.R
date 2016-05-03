#' Z-score
#'
#' Scale and center data, essentially the same as \code{scale()}
#' 
#' @param x vector of numbers
#' @param ... additional arguments for mean and sd
#' @export
#' 

zscore <- function(x, ...){
    x <- as.matrix(x)
    (x - mean(x, ...)) / sd(x, ...)
}

#' Robust Z-score 
#' 
#' Robust version of the Z-score using the median and MAD rather than the
#' mean and standard deviation of the original metric. It is more resiliant
#' to outliers.
#' 
#' @param x vector of numbers
#' @param ... additional arguments for median and mad
#' @export

r_zscore <- function(x, ...){
    x <- as.matrix(x)
    (x - median(x, ...)) / mad(x, ...)
}
