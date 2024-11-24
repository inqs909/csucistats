#' Extract Residuals and Influential Measures from and `lm` object
#'
#' @param object An R object that contains the results of the `lm` function.
#'
#' @export
#'

resid_df <- function(object){
  if (!inherits(object, c("lm", "formula"))){
    stop("Object must be a formula or obtained from the lm() function.")
  }

  post <- tibble::tibble(obs = 1:nrow(object$model),
                         object$model,
                         resid = stats::resid(object),
                         fitted = stats::fitted(object),
                         sresid = stats::rstandard(object),
                         hatvals = stats::hatvalues(object),
                         jackknife =  stats::rstudent(object),
                         cooks = stats::cooks.distance(object)
  )

  return(post)
}
