#' A function that creates a pie chart.
#'
#' @param mapping Aesthetics created by aes().
#' @param data The data used to create the plot.
#' @param position Set to "stack" for the pie chart.
#' @param ... A set of arguments passed down to geom_bar().
#' @param width Set to 1 for the pie chart.
#'
#' @returns A ggplot from the displays the pie chart.
#'
#' @export
#'
geom_pie <- function(
  mapping = NULL,
  data = NULL,
  position = "stack",
  ...,
  width = 1
) {
  # ensure a single bar at x = 1 (or ""), regardless of user mapping
  m <- if (is.null(mapping)) {
    ggplot2::aes(x = 1)
  } else {
    ggplot2::aes(x = 1) + mapping
  }

  list(
    ggplot2::geom_bar(
      mapping = m,
      data = data,
      stat = "count",
      position = position,
      width = width,
      ...
    ),
    ggplot2::coord_polar(theta = "y"),
    ggplot2::theme_void()
  )
}
