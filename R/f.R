#' Extract the F Statistic
#'
#' @param object An R object that is a `formula` or contains the results of the `lm` function.
#'
#' @export
#'

f <- function(object){
  if (!inherits(object, c("lm"))){
    stop("Object must be an obtained from the lm() function.")
  }
  post <- summary(object)$fstatistic[1] |> as.numeric()
  return(post)
}
