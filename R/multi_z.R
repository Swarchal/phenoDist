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
#' @param ... additional parameters is to be passed to \code{lda()}
#'
#' @return z-prime
#'
#' @importFrom MASS lda
#' @export

multi_z <- function(df, feature_cols, cmpd_col, pos, neg, ...){
	
	# subset df into temp df for lda calculation
	tmp_df <- data.frame(df[, c(featurecols, cmpd_col))
	
	# lda of feature cols separated by pos and negative control
	lda_model <- lda(cmpd_col ~ ., tmp_df)
	lda_out <- predict(lda_model, tmp_df)
	lda_1 <- lda_out$x[,1]
	
	lda_df <- data.frame(lda_1, df[, cmpd_col])

	# z-prime of the lda values between pos and negative control
}
