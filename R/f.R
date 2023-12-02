#' f
#'
#' @param object Model to extract F Statistic
#'
#' @export
#'

f <- function(object){
  summary(object)$fstatistic[1] |> as.numeric()
}
