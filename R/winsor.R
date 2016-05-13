#' winsorise
#'
#' Returns a winsorised vector of numbers.
#'
#' @param x vector
#' @param trim float proportion of trim at either end of distribution.
#'	expected value between 0 and 0.5
#' @param na.rm boolean, whether to remove NA values

winsor_ <- function (x, trim = 0.2, na.rm = TRUE) {

    if ((trim < 0) | (trim > 0.5)) {
	stop("trimming must be reasonable")
    }
    qtrim <- quantile(x, c(trim, 0.5, 1 - trim), na.rm = na.rm)
    xbot <- qtrim[1]
    xtop <- qtrim[3]
    if (trim < 0.5) {
	x[x < xbot] <- xbot
	x[x > xtop] <- xtop
    } else {
	x[!is.na(x)] <- qtrim[2]
     }
    return(x)
}

#' Winsorise
#'
#' Winsorise a vector or matrix of numbers
#'
#' @param x vector or matrix
#' @param trim float proportion of trim at either end of distribution.
#'	expected value between 0 and 0.5
#' @param na.rm boolean, whether to remove NA values
#' @export

winsorise <- function (x, trim = 0.2, na.rm = TRUE) {

    if (is.vector(x)) {
	ans <- winsor_(x, trim = trim, na.rm = na.rm)
    } else {
	if (is.matrix(x) | is.data.frame(x)) {
	ans <- apply(x, 2, winsor_, trim = trim, na.rm = na.rm)
    }
    return(ans)
}

#' winsorise mean
#' 
#' winsorised mean
#'
#' @param x vector of numbers
#' @param trim float, proportion of trim at either end of distribution,
#'	expectedd value between 0 and 0.5
#' @param na.rm boolean, whether to remove NA values
#' @export

winsorise_mean <- function(x, trim, na.rm = TRUE) {   
    mean(winsorise(x, trim, na.rm))
}
