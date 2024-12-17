#' Obtain the R-Squared Value from a Linear Model
#'
#' @param object An R object that is a `formula` or contains the results of the `lm` function.
#'
#' @export
#'

r2 <- function(object){
  if (!inherits(object, c("lm"))){
    stop("Object must be obtained from the lm() function.")
  }
  post <- summary(object)$r.squared
  return(post)
}
