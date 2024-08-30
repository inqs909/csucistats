#' Obtain Numerical Statistics for a Continuous Variable
#'
#' @param x A numerical or integer vector.
#' @param tbl A logical indicating whether to return a tibble or not, defaults to TRUE.
#'
#' @export
#' @importFrom stats quantile median sd var IQR
#'

num_stats <- function(x, tbl = TRUE){
  if (!(is.vector(x) & is.numeric(x))){
    stop("The supplied vector or variable must be numeric. Use the dollar sign code on your data set.")
  }
  post <- tibble::tibble(min = min(x, na.rm = T),
    q25 = quantile(x, na.rm = T, probs = 0.25) |>  as.numeric(),
    mean = mean(x, na.rm = T),
    median = median(x, na.rm = T),
    q75= quantile(x, na.rm = T, probs = 0.75) |> as.numeric(),
    max = max(x, na.rm = T),
    sd = sd(x, na.rm = T),
    var = var(x, na.rm = T),
    iqr = IQR(x, na.rm = T),
    missing = sum(is.na(x))) |> round(digits = 3)
  if(tbl){
    return(post)
  } else {
    return(unlist(post))
  }
}


#' Obtain Frequencies and Proportions for a Categorical Variable
#'
#' @param x Vector
#' @param y Vector for Cross-tabulations.
#' @param prop Character indicating what type of proportions to provide. Defaults to "all".
#' @param df Logical indicating to provide a tibble for cross tabulations with table proportions.
#'
#' @export
#'
#' @importFrom gmodels CrossTable
#'
cat_stats <- function(x, y = NULL, prop = "all", df = FALSE){
  if (!prop %in% c("all", "row", "col", "table")){
    stop("The prop argument must either be 'all', 'row', 'col', 'table'.")
  }
  if (is.null(y)){
    px <- x |> as.character() |> as.vector()
    tbl <- table(px)
    ptbl <- prop.table(tbl) |> round(digits = 3)
    miss <- sum(is.na(px))
    pmiss <- miss / length(px) |>  round(digits = 3)
    post <- tbl |> tibble::as_tibble() |>
      tibble::add_column(prop = as.numeric(ptbl)) |>
      tibble::add_row(px = "total", n = length(x) - miss, prop = NA) |>
      tibble::add_row(px = "mising", n = miss, prop = NA) |>
      tibble::add_row(px = "overall total", n = length(x), prop = NA)
    colnames(post) <- c("Category", "n", "prop")
  } else {
    if (!df){
      px <- x |> as.character() |> as.vector()
      py <- y |> as.character() |> as.vector()
      if (prop == "table"){
        post <- gmodels::CrossTable(px, py,  digits=1,
                                    prop.r=FALSE,
                                    prop.c=FALSE,
                                    prop.t=TRUE,
                                    format=c("SPSS"),
                                    dnn = c(paste(deparse(substitute(x))),
                                            paste(deparse(substitute(y))))
                                    )
      }
      if (prop == "row") {
        post <- gmodels::CrossTable(px, py,  digits=1,
                                    prop.r=TRUE,
                                    prop.c=FALSE,
                                    prop.t=FALSE,
                                    format=c("SPSS"),
                                    dnn = c(paste(deparse(substitute(x))),
                                            paste(deparse(substitute(y))))
        )
      }
      if (prop == "col") {
        post <- gmodels::CrossTable(px, py,  digits=1,
                                    prop.r=FALSE,
                                    prop.c=TRUE,
                                    prop.t=FALSE,
                                    format=c("SPSS"),
                                    dnn = c(paste(deparse(substitute(x))),
                                            paste(deparse(substitute(y))))
        )
      }
      if (prop == "all") {
        post <- gmodels::CrossTable(px, py,  digits=1,
                                    prop.r=TRUE,
                                    prop.c=TRUE,
                                    prop.t=TRUE,
                                    format=c("SPSS"),
                                    dnn = c(paste(deparse(substitute(x))),
                                            paste(deparse(substitute(y))))
        )
      }
    } else {
      wdf <- tibble::tibble(px, py)
      nn <- nrow(wdf)
      wdf |> dplyr::group_by(px, py) |>
        dplyr::summarise(n = dplyr::n(),
                         table_prop = dplyr::n() / nn)
    }
  }
  return(post)
}


#' Obtain Descriptive Statistics from a data frame.
#'
#' @param df An R data frame used for further analysis.
#'
#' @export
#'
#'
descriptive <- function(df){
  if (!is.data.frame(df)){
    stop("The object 'df' must be a data frame.")
  }
  df_numeric <- df |> Filter(is.numeric, x = _)
  col_names_numeric <- names(df_numeric)
  df_character <- df |> (\(.) {cbind(Filter(is.character, .), Filter(is.factor, .))})()
  col_names_character <- names(df_character)

  numeric <- NULL
  characters <- NULL

  if (length(col_names_numeric) != 0){
    numeric <- df_numeric |>
      sapply(num_stats, tbl = FALSE) |>
      t() |>
      tibble::as_tibble(rownames = "Variable")
  }

  if (length(col_names_character) != 0){
    characters <- df_character |> lapply(cat_stats)
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

