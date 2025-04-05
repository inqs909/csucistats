#' Obtain Numerical Statistics for a Continuous Variable
#'
#' @param x A numerical or integer vector.
#' @param tbl A logical indicating whether to return a tibble or not, defaults to TRUE.
#' @param digits number of decimal places to round
#'
#' @export
#' @importFrom stats quantile median sd var IQR
#'

num_stats <- function(x, tbl = TRUE, digits = 3){
  if (!is.numeric(x)){
    stop("The supplied vector or variable must be numeric.")
  }
  if (!is.vector(x)){
    stop("Not a vector. Use the dollar sign code on your data set.")
  }
  post <- data.frame(min = min(x, na.rm = T),
    q25 = quantile(x, na.rm = T, probs = 0.25) |>  as.numeric(),
    mean = mean(x, na.rm = T),
    median = median(x, na.rm = T),
    q75= quantile(x, na.rm = T, probs = 0.75) |> as.numeric(),
    max = max(x, na.rm = T),
    sd = sd(x, na.rm = T),
    var = var(x, na.rm = T),
    iqr = IQR(x, na.rm = T),
    missing = sum(is.na(x))) |> round(digits = digits)
  if(tbl){
    return(post)
  } else {
    return(unlist(post))
  }
}

#' Obtain Numerical Statistics for a Continuous Variable by a Categorical Variable
#'
#' @param df An R data frame used for further analysis.
#' @param num Name of the numerical variable found in the data frame
#' @param cat Name of the categorical variable found in the data frame
#'
#' @export
#'
num_by_cat_stats <- function(df, num, cat){
  if (!is.data.frame(df)){
    stop("The object 'df' must be a data frame.")
  }
  num <- substitute(num)
  cat <- substitute(cat)
  post <- df |>
    (\(x) tapply(eval(num, df), eval(cat, df), num_stats, tbl = F))() |>
    rbind.data.frame() |>
    t() |>
    as.data.frame()
  names(post) <- c("min", "q25", "mean", "median", "q75", "max", "sd", "var", "iqr", "missing")
  post <- cbind.data.frame(Categories = rownames(post), post) |> data.frame(row.names = NULL)
  return(post)
}


#' Obtain Frequencies and Proportions for a Categorical Variable
#'
#' @param x Vector
#' @param y Vector for Cross-tabulations.
#' @param prop Character indicating what type of proportions to provide. Defaults to "all".
#' @param df Logical indicating to provide a tibble for cross tabulations with table proportions.
#' @param tbl_df Logical indicating if you need a df for a single variable for a pie chart or other things.
#' @param digits number of decimal places to round
#'
#' @export
#'
#' @importFrom utils capture.output
#'
#'
cat_stats <- function(x, y = NULL, prop = "all", df = FALSE, tbl_df = FALSE, digits = 4){
  if (!prop %in% c("all", "row", "col", "table")){
    stop(paste(capture.output({
      cat("The Prop argument can only be on of the following: \n")
      cat("all: row, column, and table proportions are provided \n")
      cat("row: row proportions are provided \n")
      cat("col: column proportions are provided \n")
      cat("table: table proportions are provided \n")
    }), collapse = "\n"))
  }
  if (is.null(y)){
    px <- x |> as.character() |> as.vector()
    tbl <- table(px)
    ptbl <- prop.table(tbl)
    miss <- sum(is.na(px))
    pmiss <- miss / length(px)
    if (tbl_df){
      post_table <- tbl |> as.data.frame() |>
        cbind.data.frame(prop = round(as.numeric(ptbl), digits = digits)) 
      colnames(post_table) <- c("Category", "n", "prop")
      post <- list(table = post_table, categories = unique(px), missing = miss)
      return(post)
    } else {
      post <- cbind(tbl, round(ptbl, 4))
      colnames(post) <- c("n", "prop")
      message(paste(capture.output({
        cat("Continguency Table \n \n")
        print(post)
        cat("\n")
        cat(paste0("Number of Missing: ", miss, "\n"))
        cat(paste0("Proportion of Missing: ", pmiss, "\n"))
        cat(paste0("Row Variable:\ " , deparse(substitute(x))))
      }), collapse = "\n"))
    }
  } else {
    xtab <- table({{x}}, {{y}})
    tbl_p <- prop.table(xtab) |> round(4)
    tbl_r <- prop.table(xtab, margin = 1) |> round(digits)
    tbl_c <- prop.table(xtab, margin = 2) |> round(digits)
    tots <- sum(xtab)
    c_tot <- colSums(xtab)
    r_tot <- rowSums(xtab)
    c_props <- c_tot / tots
    r_props <- r_tot / tots

    if (prop == "all"){
      props_tbl <- t(sapply(1:nrow(xtab), \(i) {paste(xtab[i,], tbl_p[i,], tbl_r[i,], tbl_c[i,], sep = " / ")}))
      cols_tbl <- paste(c_tot, round(c_props, digits), sep =  " / ")
      rows_tbl <- paste(r_tot, round(r_props, digits), sep =  " / ")
      tbl_pdf <- rbind(cbind(props_tbl, rows_tbl), c(cols_tbl, paste("Total:", tots)))
      colnames(tbl_pdf) <- c(names(c_tot), "Row Totals")
      rownames(tbl_pdf) <- c(names(r_tot), "Col Totals")

      message(paste(capture.output({
        cat("Continguency Table \n \n")
        print(tbl_pdf)
        cat("\n")
        cat("Cell Contents: n / tbl % / row % / col % \n")
        cat("Col Totals Contents: n / row % \n")
        cat("Row Totals Contents: n / col % \n")
        cat(paste0("Column Variable:\ " , deparse(substitute(y)), "\n"))
        cat(paste0("Row Variable:\ " , deparse(substitute(x))))
      }), collapse = "\n"))
    } else if (prop == "row") {
      props_tbl <- t(sapply(1:nrow(xtab), \(i) {paste(xtab[i,], tbl_r[i,], sep = " / ")}))
      cols_tbl <- paste(c_tot, round(c_props, digits), sep =  " / ")
      rows_tbl <- paste(r_tot, round(r_props, digits), sep =  " / ")
      tbl_pdf <- rbind(cbind(props_tbl, rows_tbl), c(cols_tbl, paste("Total:", tots)))
      colnames(tbl_pdf) <- c(names(c_tot), "Row Totals")
      rownames(tbl_pdf) <- c(names(r_tot), "Col Totals")

      message(paste(capture.output({
        cat("Continguency Table \n \n")
        print(tbl_pdf)
        cat("\n")
        cat("Cell Contents: n / row % \n")
        cat("Col Totals Contents: n / row % \n")
        cat("Row Totals Contents: n / col % \n")
        cat(paste0("Column Variable:\ " , deparse(substitute(y)), "\n"))
        cat(paste0("Row Variable:\ " , deparse(substitute(x))))
      }), collapse = "\n"))
    } else if (prop == "col") {
      props_tbl <- t(sapply(1:nrow(xtab), \(i) {paste(xtab[i,], tbl_c[i,], sep = " / ")}))
      cols_tbl <- paste(c_tot, round(c_props, digits), sep =  " / ")
      rows_tbl <- paste(r_tot, round(r_props, digits), sep =  " / ")
      tbl_pdf <- rbind(cbind(props_tbl, rows_tbl), c(cols_tbl, paste("Total:", tots)))
      colnames(tbl_pdf) <- c(names(c_tot), "Row Totals")
      rownames(tbl_pdf) <- c(names(r_tot), "Col Totals")

      message(paste(capture.output({
        cat("Continguency Table \n \n")
        print(tbl_pdf)
        cat("\n")
        cat("Cell Contents: n / col % \n")
        cat("Col Totals Contents: n / row % \n")
        cat("Row Totals Contents: n / col % \n")
        cat(paste0("Column Variable:\ " , deparse(substitute(y)), "\n"))
        cat(paste0("Row Variable:\ " , deparse(substitute(x))))
      }), collapse = "\n"))
    } else {
      props_tbl <- t(sapply(1:nrow(xtab), \(i) {paste(xtab[i,], tbl_p[i,], sep = " / ")}))
      cols_tbl <- paste(c_tot, round(c_props, digits), sep =  " / ")
      rows_tbl <- paste(r_tot, round(r_props, digits), sep =  " / ")
      tbl_pdf <- rbind(cbind(props_tbl, rows_tbl), c(cols_tbl, paste("Total:", tots)))
      colnames(tbl_pdf) <- c(names(c_tot), "Row Totals")
      rownames(tbl_pdf) <- c(names(r_tot), "Col Totals")

      message(paste(capture.output({
        cat("Continguency Table \n \n")
        print(tbl_pdf)
        cat("\n")
        cat("Cell Contents: n / tbl % \n")
        cat("Col Totals Contents: n / row % \n")
        cat("Row Totals Contents: n / col % \n")
        cat(paste0("Column Variable:\ " , deparse(substitute(y)), "\n"))
        cat(paste0("Row Variable:\ " , deparse(substitute(x))))
      }), collapse = "\n"))
    }
  }
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
      as.data.frame(rownames = "Variable")
  }

  if (length(col_names_character) != 0){
    characters <- df_character |> lapply(cat_stats, tbl_df = TRUE)
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

