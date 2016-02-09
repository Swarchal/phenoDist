#' Multivariate Z-prime
#'
#' Multivarite z-score as decribed by Kummel et al. Using a LDA projection of
#' the multivariate data for the positive and negative control.
#'
#' @param df dataframe
#' @param feature_cols indices of the feature columns within \code{df}
#' @param cmpd_col index or column name of column within df that contains
#'    the positive and negative control
#' @param pos name of positive control within cmpd_col
#' @param neg name of negative control within cmpd_col
#' 
#' @return z-prime
#'
#' @importFrom MASS lda
#' @export

multi_z <- function(df, feature_cols, cmpd_col, pos, neg){
	
	# subset df into temp df for lda calculation
	tmp_df <- data.frame(df[, c(featurecols, cmpd_col))
	
	# lda of feature cols separated by pos and negative control
}
