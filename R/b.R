#' Title
#'
#' @param model Model to extract coefficients.
#' @param index Index indicating which coefficients to obtain.
#'
#' @export
#'

b <- function(model, index = NULL){
  if (is.null(index)){
    post <- stats::coef(model)
  } else if (length(index) == 1){
    post <- stats::coef(model)[index + 1] |> as.numeric()
  } else {
    post <- stats::coef(model)[index + 1]
  }
  return(post)
}
