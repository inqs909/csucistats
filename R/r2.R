#' r2
#'
#' @param object An R object that contains the results of the `lm` function.
#'
#' @export
#'

r2 <- function(object){
  summary(object)$r.squared
}
