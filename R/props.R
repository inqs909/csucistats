#' props Computing proportions
#'
#' Compute the proportions of observing a value from a 2 by 2 continguency table
#'
#' @param x grouping variable
#' @param y outcome of interest
#' @param yval category of interest from outcome of interest
#' @param diff Obtain the difference in proportions. Default is FALSE.
#'
#'
#' @export
#'
props <- function(x, y, yval, diff = FALSE){
  n <- p <- NULL
  df <- data.frame(x = as.character(x), y = as.character(y))
  pre <- table(df$x, df$y) |> prop.table(margin = 1)
  res <- pre[,as.character(yval)]

  if (isTRUE(diff)) {
    post <- as.numeric(diff(res))
  } else {
    post <- res
  }
  return(post)
}
