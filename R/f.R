#' Extract the F Statistic
#'
#' @param object An R object that contains the results of the `lm` function.
#'
#' @export
#'

f <- function(object){
  summary(object)$fstatistic[1] |> as.numeric()
}
