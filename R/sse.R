#' sse
#'
#' @param object An R object that contains the results of the `lm` function.
#'
#' @export
#'
sse <- function(object){
  sum(stats::resid(object)^2)
}
