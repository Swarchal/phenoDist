#' Z-prime / Z-factor
#'
#' Calculates #' Calculates a Z-factor for two distributions
#' 
#' Calculates a Z-factor of Z-prime for two distributions, used to assess the
#' separation between positive and negative controls in high-throughput screens.
#' A value > 0.5 is indicative of a strong assay.
#' 
#' @param positive Vector
#' @param negative Vector
#' 
#' @return z-factor
#'
#' @examples
#' x <- rnorm(100, 100)
#' y <- rnorm(100, 10)
#' z_factor(x, y)


z_factor <- function(positive, negative){

  # calculates the z-factor between selected upper and lower bound groups
  # a value between 0.5 and 1 is considered robust

  positive <- as.vector(positive)
  negative <- as.vector(negative)

  mu_p <- mean(positive, na.rm = TRUE)
  mu_n <- mean(negative, na.rm = TRUE)

      # idiot proof error handling for input order
      # assigns input with greatest mean as upper group, smaller mean -> lower group
    if (mu_n > mu_p){
      mu_p <- mean(negative, na.rm = TRUE)
      mu_n <- mean(positive, na.rm = TRUE)
    }

  sigma_p <- sd(positive)
  sigma_n <- sd(negative)

  z_prime <- 1 - ((3*(sigma_p + sigma_n)) / (mu_p - mu_n))
  return(z_prime)
}





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
	
	# subset df into temp df for lda calculation
	tmp_df <- data.frame(df[, c(featurecols, cmpd_col)])
	
	# subset only positive and negative controls from df
	compounds <- c(pos, neg)
	tmp_df_subset <- tmp_df[tmp_df[, cmpd_col] %in% compounds, ]
	
	# lda of feature cols separated by pos and negative control
	lda_model <- lda(cmpd_col ~ ., tmp_df_subset)
	lda_out <- predict(lda_model, tmp_df_subset)
	lda_1 <- lda_out$x[,1]
	
	lda_df <- data.frame(lda_1, tmp_df_subset[, cmpd_col])

	# z-prime of the lda values between pos and negative control
	pos_control <- lda_df[lda_df[, cmpd_col] == pos, 1]
	neg_control <- lda_df[lda_df[, cmpd_col] == neg, 1]
	z <- z_prime(pos_control, neg_control)
	
	return(z)
}
