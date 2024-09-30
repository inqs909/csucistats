#' Obtain the adjusted R-Squared Value from a Linear Model
#'
#' @param object An R object that is a `formula` or contains the results of the `lm` function.
#' @param data A data frame when the object is a formula.
#'
#' @export
#'

ar2 <- function(object, data = NULL){
  if (!inherits(object, c("lm", "formula"))){
    stop("Object must be a formula or obtained from the lm() function.")
  }
  if (inherits(object, "formula")){
    if (!is.data.frame(data)){
      stop("Must supply a data frame in the data argument.")
    }
    post <- summary(stats::lm(object, data = data))$adj.r.squared
  } else {
    post <- summary(object)$adj.r.squared
  }
  return(post)
}
