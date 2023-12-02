#' Sample a data frame with replacement
#'
#' @param df Data frame to be sampled with replacement
#'
#' @export
#'
resample <- function(df){
  sample(df, replace = T)
}

