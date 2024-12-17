#' Repeat a task numerous times
#'
#' @param expr Expression to be evaluated multiple times.
#' @param sim The number of times to evaluate an the expression.
#'
#' @export
#'
iterate <- function(expr, sim){
  first <- eval(expr)
  if (!is.numeric(first) & !length(first)){
    stop("Make sure the expression gives you only one number.")
  } else {
   post <- replicate(sim, expr)
  }
  return(data.frame(sim = post))
}

