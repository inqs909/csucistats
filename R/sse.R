#' sse
#'
#' @param object Object to extract the sum of error squared.
#'
#' @export
#'
sse <- function(object){
  sum(stats::resid(object)^2)
}
