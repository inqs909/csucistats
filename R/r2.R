#' r2
#'
#' @param object Model used to extract R-Squared Value
#'
#' @export
#'

r2 <- function(object){
  summary(object)$r.squared
}
