#' exapand grid, no equals
#'
#' Like expand.grid but does not list repeats
#'
#' @param x vector
#' @param y vector
#' @param include.equals whether to include duplicates, if TRUE will behave
#'   just like expand.grid with two vectors
#' @return dataframe
#' @export

expand_grid_unique <- function (x, y, include.equals = FALSE) 
{
    x <- unique(x)
    y <- unique(y)
    g <- function(i) {
        z <- setdiff(y, x[seq_len(i - include.equals)])
        if (length(z)) 
            cbind(x[i], z, deparse.level = 0)
    }
    do.call(rbind, lapply(seq_along(x), g))
}



#' Calculate pairs of cosine similarities between replicates
#'
#' Given a list, with an element per compound, columns \code{A} and \code{B}
#' are the elements of the vector with which to calculate the cosine
#' similarity
#' @param x list
#' @param a name of column for first vector element
#' @param b name of column for second vector element
#'
#' @return dataframe of compound combinations across replicates with a column 
#'    of cosine similarity values
#'
#' @export
#'
#' @examples
#' cmpds <- c(rep('a', 100), rep('b', 100), rep('c', 100))
#' replicate <- rep(1:100, 3)
#' PC1 <- rnorm(300)
#' PC2 <- rnorm(300)
#' 
#' df <- data.frame(cmpds, replicate, PC1, PC2)
#' 
#' df_split <- split(df, df$cmpds)
#' 
#' # works with unequal replicate sizes
#' df_split$a <- df_split$a[-c(1:10), ]
#' 
#' cosine_pairs(df_split, 'PC1', 'PC2')


cosine_pairs <- function(x, a, b){
  
  if (!is.list(x) || is.data.frame(x)){
    stop("Expecting a list")
  }
  
  # initialise empty vectors
  vals <- numeric()
  A <- character()
  B <- character()
  
  # get pairs of compounds
  name <- names(x)
  pairs_names <- expand_grid_unique(name, name)
  
  for (i in 1:nrow(pairs_names)){
    tmp1 <- x[[pairs_names[i, 1]]]
    tmp2 <- x[[pairs_names[i, 2]]]
    
    # loop through rows in cmpd A and cmpd B
    # calculate the cosine similarity between the two vectors
    for (j in 1:nrow(tmp1)){
      for (k in 1:nrow(tmp2)){
        vals <- c(vals, cosine_sim_vector(
          c(tmp1[j, a], tmp1[j, b]),
          c(tmp2[k, a], tmp2[k, b])))
        
        A <- c(A, pairs_names[i, 1])
        B <- c(B, pairs_names[i, 2])
      }
    }
  }
	df <- data.frame(A, B, vals)
	return(df)
}