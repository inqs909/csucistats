#' Extract regression coefficients from a linear regression model.
#'
#' The regression coefficients demonstrates how a set of predictor
#' variables will affect the outcome of interest.
#'
#' @param object An R object that is a formula or contains the results of the `lm` function.
#' @param index Index indicating which coefficients to obtain.
#' @param data A data frame when the object is a formula.
#'
#' @export
#'

b <- function(object, index = NULL, data = NULL){
  if (!inherits(object, c("lm", "formula"))){
    stop("Object must be a formula or obtained from the lm() function.")
  }
  if (inherits(object, "formula")){
    if (!is.data.frame(data)){
      stop("Must supply a data frame in the data argument.")
    }
    if (is.null(index)){
      post <- stats::coef(stats::lm(object, data = data))
    } else if (length(index) == 1){
      post <- stats::coef(stats::lm(object, data = data))[index + 1] |> as.numeric()
    } else {
      post <- stats::coef(stats::lm(object, data = data))[index + 1]
    }
  } else {
  if (is.null(index)){
    post <- stats::coef(object)
  } else if (length(index) == 1){
    post <- stats::coef(object)[index + 1] |> as.numeric()
  } else {
    post <- stats::coef(object)[index + 1]
  }
  }
  return(post)
}


#' Extract the standard errors of the regression coefficients from a linear regression model.
#'
#' The standard errors of the regression coefficients demonstrates the variability of the relationship
#' between the predictor variables and the outcome of interest.
#'
#' @param object An R object that is a formula or contains the results of the `lm` function.
#' @param index Index indicating which coefficients to obtain.
#' @param data A data frame when the object is a formula.
#'
#' @export
#'

se_b <- function(object, index = NULL, data = NULL){
  if (!inherits(object, c("lm", "formula"))){
    stop("Object must be a formula or obtained from the lm() function.")
  }
  if (inherits(object, "formula")){
    if (!is.data.frame(data)){
      stop("Must supply a data frame in the data argument.")
    }
    if (is.null(index)){
      post <- stats::coef(stats::lm(object, data = data))[, "Std. Error"]
    } else if (length(index) == 1){
      post <- stats::coef(stats::lm(object, data = data))[index + 1, "Std. Error"] |> as.numeric()
    } else {
      post <- stats::coef(stats::lm(object, data = data))[index + 1, "Std. Error"]
    }
  } else {
  if (is.null(index)){
    post <- stats::coef(object)
  } else if (length(index) == 1){
    post <- stats::coef(object)[index + 1, "Std. Error"] |> as.numeric()
  } else {
    post <- stats::coef(object)[index + 1, "Std. Error"]
  }
  }
  return(post)
}