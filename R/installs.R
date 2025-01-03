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
  message("Installed the following packages:\n ggplot2\n waffle\n ggmosaic\n ggtricks \n ggtext")
}


#' Install R packages used for Math 201
#'
#' @export
#'
#' @importFrom utils install.packages
#' @importFrom remotes install_github
#'

install_m201 <- function(){
  install.packages(c("waffle", "ggmosaic", 
                     "ggtricks", "ggtext", "ggthemes",
                     "tidyverse", "patchwork", "openintro",
                     "palmerpenguins", "taylor", "DT"))
  remotes::install_github("MatthewBJane/ThemePark", force = TRUE, upgrade = "never")
  message(paste(capture.output({
    cat("Installed the following packages:")
    cat("\n tidyverse")
    cat(paste0("\n patchwork", "\n openintro", "\n palmerpenguins"))
    cat(paste0("\n waffle\n ggmosaic\n ggtricks \n ggtext"))
    cat(paste0("ggthemes \n ThemePark \n taylor \n DT"))
  }), collapse = "\n"))
}




