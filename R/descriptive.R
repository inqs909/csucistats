#' Obtain Numerical Statistics From a Vector
#'
#' @param x Vector
#'
#' @export
#' @importFrom stats quantile median sd var IQR
#'

num_stats <- function(x){
  c(min = min(x, na.rm = T),
    q25 = quantile(x, na.rm = T, probs = 0.25) |>  as.numeric(),
    mean = mean(x, na.rm = T),
    median = median(x, na.rm = T),
    q75= quantile(x, na.rm = T, probs = 0.75) |> as.numeric(),
    max = max(x, na.rm = T),
    sd = sd(x, na.rm = T),
    var = var(x, na.rm = T),
    iqr = IQR(x, na.rm = T),
    missing = sum(is.na(x))) |> round(digits = 3)
}


#' Obtain Frequencies and Proportions for a Categorical Variable
#'
#' @param x Vector
#'
#' @export
#'
char_stats <- function(x){
  tbl <- table(x)
  ptbl <- prop.table(tbl) |> round(digits = 3)
  miss <- sum(is.na(x))
  pmiss <- miss / length(x) |>  round(digits = 3)
  post <- tbl |> tibble::as_tibble() |>
    tibble::add_column(prop = as.numeric(ptbl)) |>
    tibble::add_row(x = "mising", n = miss, prop = pmiss)
  colnames(post) <- c("Category", "n", "prop")
  return(post)
}


#' Obtain Descriptive Statistics from a data frame.
#'
#' @param df an R data frame used for further analysis.
#'
#' @export
#'
#'
descriptive <- function(df){
  df_numeric <- df |> Filter(is.numeric, x = _)
  col_names_numeric <- names(df_numeric)
  df_character <- df |> (\(.) {cbind(Filter(is.character, .), Filter(is.factor, .))})()
  col_names_character <- names(df_character)
  df_character |> sapply(table)

  numeric <- NULL
  characters <- NULL

  if (length(col_names_numeric) != 0){
    numeric <- df_numeric |> sapply(num_stats) |> t() |> tibble::as_tibble(rownames = "Variable")
  }

  if (length(col_names_character) != 0){
    characters <- df_character |> lapply(char_stats)
  }
  if (!is.null(numeric) & !is.null(characters)){
    post <- list(Numerical = numeric, Categorical = characters)
  } else if (!is.null(numeric) & is.null(characters)){
    post <- list(Numeric = numeric)
  } else {
    post <- list(Categorical = characters)
  }

  return(post)
}

