#' Compute the sum of error squared for an R object
#'
#' @param object An R object that is a `formula` or contains the results of the `lm` function.
#' @param data A data frame when the object is a formula.
#'
#' @export
#'
sse <- function(object, data = NULL){
  if (!inherits(object, c("lm"))){
    stop("Object must be obtained from the lm() function.")
  }
  post <- sum(stats::resid(object)^2)
  return(post)
}
