#' Install Themes
#'
#' @export
#'
#' @importFrom remotes install_github
#'

install_themes <- function(){
  install.packages(c("ggthemes"))
  remotes::install_github("MatthewBJane/ThemePark", force = TRUE, upgrade = "never")
  message("Installed the following themes:\n ggthemes \n ThemePark")
}



#' Install ggplots and other plotting packages
#'
#' @export
#'
#' @importFrom utils install.packages
#'
install_plots <- function(){
  install.packages(c("ggplot2", "waffle", "ggmosaic", "ggtricks", "ggtext"))
  remotes::install_github("R-CoderDotCom/ggcats@main", force = TRUE, upgrade = "never")
  message("Installed the following packages:\n ggplot2\n waffle\n ggmosaic\n ggtricks\n ggcats")
}


