#' Compute the sum of error squared for an R object
#'
#' @param object An R object that is a `formula` or contains the results of the `lm` function.
#' @param data A data frame when the object is a formula.
#'
#' @export
#'
sse <- function(object, data = NULL){
  if (!inherits(object, c("lm", "formula"))){
    stop("Object must be a formula or obtained from the lm() function.")
  }
  if (inherits(object, "formula")){
    if (!is.data.frame(data)){
      stop("Must supply a data frame in the data argument.")
    }
    post <- sum(stats::resid(stats::lm(object, data = data))^2)
  } else {
    post <- sum(stats::resid(object)^2)
  }
  return(post)
}
