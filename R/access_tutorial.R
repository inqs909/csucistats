#' access_tutorial
#'
#' A wrapper function to deploy a learnr tutorial in the csucistats package.
#'
#' @param tutorial Name of the tutorial to be accessed.
#'
#' @export
#'
access_tutorial<-function(tutorial){
  learnr::run_tutorial(name=tutorial, package = "csucistats")
}
