#' Compute the sum of error squared for an R object
#'
#' @param object An R object that contains the results of the `lm` function.
#'
#' @export
#'
sse <- function(object){
  if (!inherits(object, "lm")){
    stop("Object must be obtained from the lm() function.")
  }
  sum(stats::resid(object)^2)
}
