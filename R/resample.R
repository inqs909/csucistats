#' Sample a data frame with replacement
#'
#' @param df Data frame to be sampled with replacement
#'
#' @export
#'
resample <- function(df){
  if (!is.data.frame(df)){
    stop("The df object must be a data frame.")
  }
  dplyr::slice_sample(df, n = nrow(df), replace = T )
}

