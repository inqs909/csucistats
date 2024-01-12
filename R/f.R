#' Extract the F Statistic
#'
#' @param object An R object that is a `formula` or contains the results of the `lm` function.
#' @param data A data frame when the object is a formula.
#'
#' @export
#'

f <- function(object, data = NULL){
  if (!inherits(object, c("lm", "formula"))){
    stop("Object must be a formula or obtained from the lm() function.")
  }
  if (inherits(object, "formula")){
    if (!is.data.frame(data)){
      stop("Must supply a data frame in the data argument.")
    }
    post <- summary(stats::lm(object, data = data))$fstatistic[1] |> as.numeric()
  } else {
    post <- summary(object)$fstatistic[1] |> as.numeric()
  }
  return(post)
}
