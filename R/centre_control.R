#' Shift co-ordinates to centre on a compounds centroid
#'
#' Idea is to shift co-ordinates so that the centroid of the negative control
#' is always centered on 0,0. Useful for PCA biplots where the 0,0 denotes the
#' centroid of the entire data, and not necessarily the negative control. If 
#' given a dataframe and specified columns for the x and y co-ordinates, this
#' function will shift the co-ordinates to be centered on the centroid of a
#' specified compound.
#'
#' @param df dataframe
#' @param x column of x co-ordinates
#' @param y column of y-co-ordinates
#' @param cmpd_col column containing names to subset with \code{cmpd}
#' @param cmpd name of compound to subset \code{cmpd_col} with
#'
#' @return dataframe containing x, y, cmpd_col columns shifted so that
#'   cmpd is centered on 0,0
#'
#' @export
#'
#' @examples
#' # artifical example using iris
#' pca <- prcomp(iris[,1:4])$x[,1:2]
#' df <- data.frame(pca, name = iris[,5])
#' pca_shift <- centre_control(df, x = 'PC1', y = 'PC2', cmpd_col = 'name', cmpd = 'setosa')
#' 
#' par(mfrow = c(2,1))
#' plot(df$PC1, df$PC2, col = df$name, pch = 20)
#' plot(pca_shift$PC1, pca_shift$PC2, col = pca_shift$name, pch = 20)


centre_control <- function(df, x, y, cmpd_col, cmpd){
    
    # check inputs
    if (!is.data.frame(df)){
        stop("df needs to be a dataframe")
    }
    if (!cmpd_col %in% names(df)){
        stop(paste(cmpd_col, "not a column in", substitute(df)))
    }
    if (!x %in% names(df)){
        stop(paste(x, "not a column in", substitute(df)))
    }
    if (!y %in% names(df)){
        stop(paste(y, "not a column in", substitute(df)))
    }
    if (!cmpd %in% unique(df[, cmpd_col])){
        stop(paste(cmpd, "not a compound in", cmpd_col))
    }
    
    # find cmpd centroid
    X <- mean(df[df[, cmpd_col] == cmpd, x])
    Y <- mean(df[df[, cmpd_col] == cmpd, y])
    
    # find shift from centre
    dX <- 0-X
    dY <- 0-Y
    
    # shift values in columns
    x_shift <- df[, x] + dX
    y_shift <- df[, y] + dY
    
    df[, x] <- x_shift
    df[, y] <- y_shift
    
    return(df)
    
}


#' Shift co-ordinates to centre on a compounds centroid
#'
#' Idea is to shift co-ordinates so that the centroid of the negative control
#' is always centered on 0,0. Useful for PCA biplots where the 0,0 denotes the
#' centroid of the entire data, and not necessarily the negative control. If 
#' given a dataframe and specified columns for the x and y co-ordinates, this
#' function will shift the co-ordinates to be centered on the centroid of a
#' specified compound. The difference between \code{centre_control} and 
#' \code{centre_controls_m} is that \code{centre_control_m} can centre a range
#' or columns, whereas \code{centre_control} is limited to 2 named columns.
#'
#' @param df dataframe
#' @param cols column indicies or names
#' @param cmpd_col column containing names to subset with \code{cmpd}
#' @param cmpd name of compound to subset \code{cmpd_col} with
#'
#' @return dataframe of 'df' shifted so that
#'   cmpd is centered on 0,0
#'
#' @export
#'
#' @examples
#' # artifical example using iris
#' pca <- prcomp(iris[,1:4])$x[,1:3]
#' df <- data.frame(pca, name = iris[,5])
#' pca_shift <- centre_control_m(df, 1:3,  cmpd_col = 'name', cmpd = 'setosa')
#' 
#' par(mfrow = c(2,1))
#' plot(df$PC1, df$PC2, col = df$name, pch = 20)
#' plot(pca_shift$PC1, pca_shift$PC2, col = pca_shift$name, pch = 20)

centre_control_m <- function(df, cols, cmpd_col, cmpd){

    # find DMSO centroids in each value
    centroids <- apply(df[df[, cmpd_col] == cmpd, cols], 2, mean)

    # shift from centre
    d <- 0 - centroids

    # shift values in columns
    for (col in seq_along(cols)){
	df[, cols][i] <- df[, cols][i] + d[i]
    }

    return(df)
}
