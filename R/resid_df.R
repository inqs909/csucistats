#' Extract Residuals and Influential Measures from `glm` and `lm` object
#'
#' @param object An R object that contains the results of the `lm` or `glm` function.
#'
#' @export
#'

resid_df <- function(object) {
  if (!inherits(object, c("lm", "glm"))) {
    stop("Object must be obtained from the lm() or glm() function.")
  }

  if (inherits(object, "lm") && !inherits(object, "glm")) {
    post <- data.frame(
      obs = 1:nrow(object$model),
      object$model,
      resid = stats::resid(object),
      fitted = stats::fitted(object),
      sresid = stats::rstandard(object),
      hatvals = stats::hatvalues(object),
      jackknife = stats::rstudent(object),
      cooks = stats::cooks.distance(object)
    )
  } else {
    (inherits(object, "glm"))
    post <- data.frame(
      obs = 1:nrow(object$model),
      object$model,
      fitted = stats::fitted(object),
      eta = object$linear.predictors,
      raw_resid = stats::resid(object, type = "response"),
      pearson_resid = stats::resid(object, type = "pearson"),
      deviance_resid = stats::resid(object),
      working_resid = stats::resid(object, type = "working") +
        object$linear.predictors,
      partial_resid = stats::resid(object, type = "partial"),
      std_pear_resid = stats::rstandard(object, type = "pearson"),
      std_dev_resid = stats::rstandard(object),
      stud_dev_resid = stats::rstudent(object),
      quantile_resid = statmod::qresid(object),
      leverages = stats::hatvalues(object),
      cooks = stats::cooks.distance(object)
    )
  }
  return(post)
}
