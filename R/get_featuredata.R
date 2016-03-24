#' Get featuredata columns
#'
#' Identifies columns in a dataframe that are not labelled Metadata
#'
#' @param x dataframe
#' @param metadata_prefix pattern to match metadata columns
#' @export

get_featuredata <- function(x, metadata_prefix = "Metadata"){
        setdiff(1:ncol(x), grep(metadata_prefix, colnames(x)))
}
