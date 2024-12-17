#' Obtain the adjusted R-Squared Value from a Linear Model
#'
#' @param object An R object that is a `formula` or contains the results of the `lm` function.
#'
#' @export
#'

ar2 <- function(object){
  if (!inherits(object, c("lm"))){
    stop("Object must be a formula or obtained from the lm() function.")
  }
  post <- summary(object)$adj.r.squared
  return(post)
}
