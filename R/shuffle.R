#' Reorder the data of a vector
#'
#' @param x A vector to be shuffle the order of the values.
#'
#' @export
#'
shuffle <- function(x){
  sample(x, replace = F)
}
