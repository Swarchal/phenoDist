#' Multivariate Z-prime
#'
#' Multivarite z-score as decribed by Kummel et al. Calculates a Z-prime
#' using a LDA projection of the multivariate data for the positive and
#' negative control.
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

	# check cmpd_col are factors
	df[, cmpd_col] <- as.factor(df[, cmpd_col])

	# subset df into temp df for lda calculation
	tmp_df <- data.frame(df[, feature_cols],
	                     cmpd_col = df[, cmpd_col])

	# subset only positive and negative controls from df
	compounds <- c(pos, neg)
	tmp_df_subset <- tmp_df[ tmp_df$cmpd_col %in% compounds, ]

	# drop empty factors otherwise lda throws a fit
	tmp_df_subset$cmpd_col <- factor(tmp_df_subset$cmpd_col)

	# lda of feature cols separated by pos and negative control
	lda_model <- lda(cmpd_col ~ ., tmp_df_subset, ...)
	lda_out <- predict(lda_model, tmp_df_subset)
	lda_1 <- lda_out$x[,1]

	lda_df <- data.frame(lda_1, cmpd_col = tmp_df_subset$cmpd_col)

	# z-prime of the lda values between pos and negative control
	pos_control <- lda_df[lda_df$cmpd_col == pos, 1]
	neg_control <- lda_df[lda_df$cmpd_col == neg, 1]
	z <- z_factor(pos_control, neg_control)

	return(z)
}
