#' resample
#'
#' @param df Data frame to be sampled with replacement
#'
#' @export
#'
resample <- function(df){
  sample(df, replace = T)
}

