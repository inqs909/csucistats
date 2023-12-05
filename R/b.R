#' Extract the regression coefficients
#'
#' The regression coefficients demonstrates how a set of predictor
#' variables will affect the outcome of interest
#'
#' @param object An R object that contains the results of the `lm` function.
#' @param index Index indicating which coefficients to obtain.
#'
#' @export
#'

b <- function(object, index = NULL){
  if (inherits(object, "lm")){
    stop("Object must be obtained from the lm() function.")
  }
  if (is.null(index)){
    post <- stats::coef(object)
  } else if (length(index) == 1){
    post <- stats::coef(object)[index + 1] |> as.numeric()
  } else {
    post <- stats::coef(object)[index + 1]
  }
  return(post)
}
