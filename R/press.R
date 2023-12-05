#' Repeat a task numerous times
#'
#' @param expr Expression to be evaluated multiple times.
#' @param sim The number of times to evaluate an the expression.
#'
#' @export
#'
press <- function(expr, sim){
  first <- eval(expr)
  if (!is.numeric(first) & !length(first)){
    stop("Make sure the expression gives you only one number.")
  } else {
   post <- sapply(1:sim, function(i){eval(expr)})
  }
  return(tibble::tibble(sim = post))
}

