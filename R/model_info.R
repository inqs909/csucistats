#' Extract model information for linear regression.
#'
#' @param model An R object that results from a linear regression model.
#'
#' @returns A message model information.
#'
#' @export

linear_model_info <- function(model) {
  # Check that it's a linear model
  if (!inherits(model, "lm")) {
    stop("Model must be a linear regression object (lm).")
  }

  # Get model frame (the data actually used)
  mf <- stats::model.frame(model)

  # Extract the outcome variable (first column)
  outcome_name <- names(mf)[1]

  predictors <- mf[-1]

  # Extract predictor names and values
  numeric_predictors <- names(predictors[vapply(
    predictors,
    is.numeric,
    logical(1)
  )])

  factor_predictors <- names(predictors[vapply(
    predictors,
    is.factor,
    logical(1)
  )])

  logical_predictors <- names(predictors[vapply(
    predictors,
    is.logical,
    logical(1)
  )])

  factor_predictors <- c(factor_predictors, logical_predictors)

  factor_info <- lapply(factor_predictors, function(var) {
    lvls <- levels(as.factor(predictors[[var]]))
    ref <- lvls[1] # default reference level
    list(
      levels = lvls
    )
  })

  factor_info <- lapply(factor_predictors, function(var) {
    lvls <- levels(predictors[[var]])
    ref <- lvls[1] # default reference level
    list(
      levels = lvls
    )
  })

  names(factor_info) <- factor_predictors

  for (i in 1:length(factor_predictors)) {
    factor_info[[factor_predictors[[i]]]]$levels[1] <- paste(
      factor_info[[factor_predictors[[i]]]]$levels[1],
      "(Reference)"
    )
  }
  message("Outcome Variable: ", outcome_name)
  message(paste(
    capture.output({
      cat("Numerical Predictors: \n")
      for (i in 1:length(numeric_predictors)) {
        cat(paste("  ", numeric_predictors[i], " \n"))
      }
    }),
    collapse = "\n"
  ))
  message(paste(
    capture.output({
      cat("Categorical Predictors: \n")
      for (i in 1:length(factor_predictors)) {
        cat(paste("  ", factor_predictors[i], ": \n"))
        for (j in 1:length(factor_info[[factor_predictors[[i]]]]$levels)) {
          cat(paste(
            "    ",
            factor_info[[factor_predictors[[i]]]]$levels[j],
            " \n"
          ))
        }
      }
    }),
    collapse = "\n"
  ))
}


#' Extract model information for logistic regression
#'
#' @param model An R object that results from a logistic regression model (glm class).
#'
#' @returns A message indicating which category is being modeled as success from the logistic regression model.
#'
#' @export

logistic_model_info <- function(model) {
  # Check if the model is a glm with binomial family
  if (!inherits(model, "glm") || model$family$family != "binomial") {
    stop("Model must be a logistic regression (glm with binomial family).")
  }

  # Extract the model frame
  mf <- stats::model.frame(model)

  # Extract the response variable (first column of the model frame)
  outcome <- levels(mf[[1]])[2]

  if (is.null(outcome)) {
    post <- 1
  } else {
    post <- outcome
  }

  predictors <- mf[-1]

  # Extract predictor names and values
  numeric_predictors <- names(predictors[vapply(
    predictors,
    is.numeric,
    logical(1)
  )])

  factor_predictors <- names(predictors[vapply(
    predictors,
    is.factor,
    logical(1)
  )])

  logical_predictors <- names(predictors[vapply(
    predictors,
    is.logical,
    logical(1)
  )])

  factor_predictors <- c(factor_predictors, logical_predictors)

  factor_info <- lapply(factor_predictors, function(var) {
    lvls <- levels(as.factor(predictors[[var]]))
    ref <- lvls[1] # default reference level
    list(
      levels = lvls
    )
  })

  names(factor_info) <- factor_predictors

  for (i in 1:length(factor_predictors)) {
    factor_info[[factor_predictors[[i]]]]$levels[1] <- paste(
      factor_info[[factor_predictors[[i]]]]$levels[1],
      "(Reference)"
    )
  }
  message("Outcome Variable: ", names(mf[1]))
  message("  Modeling Probability: ", post)
  message(paste(
    capture.output({
      cat("Numerical Predictors: \n")
      for (i in 1:length(numeric_predictors)) {
        cat(paste("  ", numeric_predictors[i], " \n"))
      }
    }),
    collapse = "\n"
  ))
  message(paste(
    capture.output({
      cat("Categorical Predictors: \n")
      for (i in 1:length(factor_predictors)) {
        cat(paste0("  ", factor_predictors[i], ": \n"))
        for (j in 1:length(factor_info[[factor_predictors[[i]]]]$levels)) {
          cat(paste(
            "    ",
            factor_info[[factor_predictors[[i]]]]$levels[j],
            " \n"
          ))
        }
      }
    }),
    collapse = "\n"
  ))
}
