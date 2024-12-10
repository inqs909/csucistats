#' props Computing proportions
#'
#' Compute the proportions of observing a value from a 2 by 2 continguency table
#'
#' @param df data frame
#' @param x grouping variable
#' @param y outcome of interest
#' @param yval category of interest from outcome of interest
#' @param diff Obtaine the difference in proportions. Default is FALSE.
#'
#' @import rlang
#' @export
#'
props_df <- function(df, x, y, yval, diff = FALSE){
  n <- p <- NULL
  suppressMessages(
    res <- df |> dplyr::group_by({{x}}, {{y}}) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::mutate(p = n / sum(n)) |>
      dplyr::ungroup() |>
      dplyr::filter({{y}} == yval) |>
      dplyr::select(p) |>
      unlist()
  )
  if (isTRUE(diff)) {
    post <- as.numeric(diff(res))
  } else {
    post <- res
  }
  return(post)
}
