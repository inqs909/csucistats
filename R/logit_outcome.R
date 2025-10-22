#' Extract the Category indicating Probability of Success
#'
#' @param model An R object that results from a logistic regression model (glm class).
#'
#' @returns A message indicating which category is being modeled as success from the logistic regression model.
#'
#' @export

logistic_outcome <- function(model) {
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

  return(message("The Logistic Regression Model is modeling: ", post))
}
