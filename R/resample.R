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
  index <- sample(1:nrow(df), nrow(df), TRUE)
  post <- df[index,]
  return(post)
}

