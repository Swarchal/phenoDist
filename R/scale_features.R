#' Scale feature data
#'
#' Z-score of feature data, each features scaled separately
#'
#' @param df dataframe
#' @export

scale_features <- function(df){
        feature_data <- get_featuredata(df)
    apply(df[, feature_data], 2, scale)
}
