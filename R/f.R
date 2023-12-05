#' Extract the F Statistic
#'
#' @param object An R object that contains the results of the `lm` function.
#'
#' @export
#'

f <- function(object){
  if (!inherits(object, "lm")){
    stop("Object must be obtained from the lm() function.")
  }
  summary(object)$fstatistic[1] |> as.numeric()
}
