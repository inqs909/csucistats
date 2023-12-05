#' Obtain the top X Percent
#'
#' @param x Vector
#' @param probs Probability
#'
#' @export
#'
top <- function(x, probs){
  topper <- quantile(x, probs = 1 - probs)
  return(x > topper)
}

#' Obtain the middle X Percent
#'
#' @param x Vector
#' @param probs Probability
#'
#' @export
#'
middle <- function(x, probs){
  qq <- quantile(x, probs = c(1-probs, probs))
  bottom <- x > qq[1]
  top <- x < qq[2]
  post <- bottom & top
  return(post)
}


#' Obtain the Bottom X Percents
#'
#' @param x Vector
#' @param probs Probability
#'
#' @export
#'
bottom <- function(x, probs){
  topper <- quantile(x, probs = probs)
  return(x < topper)
}

