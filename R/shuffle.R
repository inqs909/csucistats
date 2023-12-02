#' shuffle
#'
#' @param x Vecto to be shuffle the order of the values.
#'
#' @export
#'
shuffle <- function(x){
  sample(x, replace = F)
}
