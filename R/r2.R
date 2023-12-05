#' Obtained the R-Squared Value from a Linear Model
#'
#' @param object An R object that contains the results of the `lm` function.
#'
#' @export
#'

r2 <- function(object){
  if (inherits(object, "lm")){
    stop("Object must be obtained from the lm() function.")
  }
  summary(object)$r.squared
}
