#' Multiple z-factor calculations
#' 
#' Calculates z-factors between two compounds for multiple variables and returns the
#' z-factors and associated features above a specified cut-off. The cut-off can be
#' determined either by a minumum z-factor score, or return the top n features.
#' 
#' @param data Dataframe, containing only numerical columns of features and
#' 		single column of treatment labels
#' @param treatment_col string, name of column containing treatments
#' @param treatments vector of strings, names of positive and negative control.
#'	    i.e c("DMSO", "STS)
#' @param cutoff Minimum z-factor threshold for returning featues. Default is 0.5
#' @param n Highest n features to be returned
#'
#' @export
#' 


z_factor_scan <- function(data, treatment_col, treatments, cutoff = 0.5, n = FALSE){

    header_in <- which(colnames(data) == treatment_col) # index treatment_col
    nums <- (1:ncol(data))[- header_in] # indices of all columns except header
    z_values <- c() # initialise for the loop
    z_names <- c() # initialise for the loop
    
    for (number in nums){ # for each feature (except header)
	z_values[number] <- as.vector(
	    z_factor(data[data[, treatment_col] == treatments[1], number],
		     data[data[, treatment_col] == treatments[2], number])
	)
	z_names[number] <- names(data)[number]
    }
    
    # default behaviour, returns features and values above specified cutoff
    if (n == FALSE){
	z_factors <- data.frame(z_names, z_values) # create dataframe
	# subset of z_factors above cutoff
	z_factors_good <- z_factors[z_factors$z_values > cutoff, ]
	# names for dataframe columns
	names(z_factors_good)[c(1,2)] <- c("Feature", "Z_factor")
	# order dataframe from highest z-factor to the lowest
	z_out <- z_factors_good[with(z_factors_good, order(-Z_factor)), ]
    }
    
    # The 'n' argument is return the highest 'n' values for Z-factor
    # Default behavious is n = FALSE, so instead calls the cutoff value.
    if (is.logical(n) == FALSE){
        if (n == TRUE) stop("n has to be either FALSE or an integer", call. = FALSE)
        n <- as.integer(n) # forces n into an integer
        z_factors <- data.frame(z_names, z_values) # create dataframe
        names(z_factors)[c(1,2)] <- c("Feature", "Z_factor") # assign colnames
        z_factors_order <- z_factors[order(- z_factors$Z_factor), ] # re-order
        z_out <- z_factors_order[1:n, ] # subset first n values
    }
    return(z_out)
}
