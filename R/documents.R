#' Generate a QMD file to create a homework template for class. Files will be saved in it's own folder.
#'
#' @param hw_name Name of folder to store all important files for the homework assignment. This argument will remove unwanted punctuations and spaces.
#' @param name Your name.
#' @param title Optional title to give to assignment.
#' @param date Optional Date to give to the document.
#' @param dir_loc Optional location to place the folder and hw template contents.
#' @param data If TRUE, a data folder will be constructed in hw template folder.
#'
#' @export
#' @importFrom stringr str_glue
#'

new_hw <- function(hw_name,
                   name = "Name Here",
                   title = "Title Here",
                   date = "`r format(Sys.time(),'%m-%d-%Y')`",
                   dir_loc = getwd(),
                   data = FALSE
                   ){
  slug <- tolower(hw_name)
  slug <- gsub('[[:punct:] ]+',' ', slug)
  slug <- trimws(slug)
  slug <- gsub(" ", "_", slug)
  dir_name <- paste0(dir_loc, "/", slug)
  if (dir.exists(dir_name)) {
    stop("The cleaned version of the folder already exists in the current working directory.\n
         Change the name by adding more numbers or letters to 'hw_name' or changing the 'dir_loc' argument.")
  }
  dir.create(dir_name)
  file_contents <- stringr::str_glue('---
title: "{title}"
author: "{name}"
date: "{date}"
format:
  html:
    toc: true
    toc-depth: 2
    code-fold: true
    code-tools: true
    code-line-numbers: true
    embed-resources: true
    include-in-header:
      - file: "math.jax"
knitr:
  opts_chunk:
    echo: true
    message: false
    warning: false
    error: true
    tidy: styler
    R.options:
      digits: 3
      max.print: 100
---

## Problem 1

## Problem 2

## Problem 3
')
  cat(file_contents, file = paste0(dir_name,'/hw.qmd'))
  package_csucistats <- path.package("csucistats")
  mathfile <- paste0(package_csucistats, "/extras/math.jax")
  file.copy(mathfile, paste0(dir_name,'/math.jax'))
  if(data){
    dir.create(paste0(dir_name,"/data"))
  }
}


#' Generate a QMD file to create a homework template for class. Files will be saved in it's own folder. This function will automatically open in RStudio.
#'
#' @param hw_name Name of folder to store all important files for the homework assignment. This argument will remove unwanted punctuations and spaces.
#' @param name Your name.
#' @param title Optional title to give to assignment.
#' @param date Optional Date to give to the document.
#' @param dir_loc Optional location to place the folder and hw template contents.
#' @param data If TRUE, a data folder will be constructed in hw template folder.
#'
#' @export
#' @importFrom stringr str_glue
#' @importFrom rstudioapi documentOpen
#'

new_hw_open <- function(hw_name,
                        name = "Name Here",
                        title = "Title Here",
                        date = "`r format(Sys.time(),'%m-%d-%Y')`",
                        dir_loc = getwd(),
                        data = FALSE
){
  slug <- tolower(hw_name)
  slug <- gsub('[[:punct:] ]+',' ', slug)
  slug <- trimws(slug)
  slug <- gsub(" ", "_", slug)
  dir_name <- paste0(dir_loc, "/", slug)
  if (dir.exists(dir_name)) {
    stop("The cleaned version of the folder already exists in the current working directory.\n
         Change the name by adding more numbers or letters to 'hw_name' or changing the 'dir_loc' argument.")
  }
  dir.create(dir_name)
  file_contents <- stringr::str_glue('---
title: "{title}"
author: "{name}"
date: "{date}"
format:
  html:
    toc: true
    toc-depth: 2
    code-fold: true
    code-tools: true
    code-line-numbers: true
    embed-resources: true
    include-in-header:
      - file: "math.jax"
knitr:
  opts_chunk:
    echo: true
    message: false
    warning: false
    error: true
    tidy: styler
    R.options:
      digits: 3
      max.print: 100
---

## Problem 1

## Problem 2

## Problem 3
')
  cat(file_contents, file = paste0(dir_name,'/hw.qmd'))
  package_csucistats <- path.package("csucistats")
  mathfile <- paste0(package_csucistats, "/extras/math.jax")
  file.copy(mathfile, paste0(dir_name,'/math.jax'))
  if(data){
    dir.create(paste0(dir_name,"/data"))
  }
  invisible(rstudioapi::documentOpen(paste0(dir_name,'/hw.qmd')))
}


#' Generate a QMD file to create a presentation template for class. Files will be saved in it's own folder.
#'
#' @param hw_name Name of folder to store all important files for the homework assignment. This argument will remove unwanted punctuations and spaces.
#' @param name Your name.
#' @param title Optional title to give to assignment.
#' @param date Optional Date to give to the document.
#' @param dir_loc Optional location to place the folder and hw template contents.
#' @param data If TRUE, a data folder will be constructed in hw template folder.
#'
#' @export
#' @importFrom stringr str_glue
#' @importFrom rstudioapi documentOpen
#'

new_presentation <- function(hw_name,
                             name = "Name Here",
                             title = "Title Here",
                             date = "`r format(Sys.time(),'%m-%d-%Y')`",
                             dir_loc = getwd(),
                             data = FALSE
){
  slug <- tolower(hw_name)
  slug <- gsub('[[:punct:] ]+',' ', slug)
  slug <- trimws(slug)
  slug <- gsub(" ", "_", slug)
  dir_name <- paste0(dir_loc, "/", slug)
  if (dir.exists(dir_name)) {
    stop("The cleaned version of the folder already exists in the current working directory.\n
         Change the name by adding more numbers or letters to 'hw_name' or changing the 'dir_loc' argument.")
  }
  dir.create(dir_name)
  file_contents <- stringr::str_glue('---
title: "{title}"
author: "{name}"
date: "{date}"
format:
  revealjs:
    embed-resources: true
    include-in-header: "math_jax.html"
    toc: true
    toc-depth: 1
    toc-title: "Contents"
    scrollable: true
    controls-tutorial: true
    incremental: false
knitr:
  opts_chunk:
    echo: false
    message: false
    warning: false
    error: true
    tidy: styler
    R.options:
      digits: 3
      max.print: 100
---

# Heading 1

## Slide 1

## Slide 2

## Slide 3

# Heading 2

## Slide 4

## Slide 5

## Slide 6

')
  cat(file_contents, file = paste0(dir_name,'/presentation.qmd'))
  package_csucistats <- path.package("csucistats")
  mathfile <- paste0(package_csucistats, "/extras/math_jax.html")
  file.copy(mathfile, paste0(dir_name,'/math_jax.html'))
  if(data){
    dir.create(paste0(dir_name,"/data"))
  }
}




#' Generate a QMD file to create a presentation template for class. Files will be saved in it's own folder. This function will automatically open in RStudio.
#'
#' @param hw_name Name of folder to store all important files for the homework assignment. This argument will remove unwanted punctuations and spaces.
#' @param name Your name.
#' @param title Optional title to give to assignment.
#' @param date Optional Date to give to the document.
#' @param dir_loc Optional location to place the folder and hw template contents.
#' @param data If TRUE, a data folder will be constructed in hw template folder.
#'
#' @export
#' @importFrom stringr str_glue
#' @importFrom rstudioapi documentOpen
#'

new_presentation_open <- function(hw_name,
                                  name = "Name Here",
                                  title = "Title Here",
                                  date = "`r format(Sys.time(),'%m-%d-%Y')`",
                                  dir_loc = getwd(),
                                  data = FALSE
){
  slug <- tolower(hw_name)
  slug <- gsub('[[:punct:] ]+',' ', slug)
  slug <- trimws(slug)
  slug <- gsub(" ", "_", slug)
  dir_name <- paste0(dir_loc, "/", slug)
  if (dir.exists(dir_name)) {
    stop("The cleaned version of the folder already exists in the current working directory.\n
         Change the name by adding more numbers or letters to 'hw_name' or changing the 'dir_loc' argument.")
  }
  dir.create(dir_name)
  file_contents <- stringr::str_glue('---
title: "{title}"
author: "{name}"
date: "{date}"
format:
  revealjs:
    embed-resources: true
    include-in-header: "math_jax.html"
    toc: true
    toc-depth: 1
    toc-title: "Contents"
    scrollable: true
    controls-tutorial: true
    incremental: false
knitr:
  opts_chunk:
    echo: false
    message: false
    warning: false
    error: true
    tidy: styler
    R.options:
      digits: 3
      max.print: 100
---

# Heading 1

## Slide 1

## Slide 2

## Slide 3

# Heading 2

## Slide 4

## Slide 5

## Slide 6

')
  cat(file_contents, file = paste0(dir_name,'/presentation.qmd'))
  package_csucistats <- path.package("csucistats")
  mathfile <- paste0(package_csucistats, "/extras/math_jax.html")
  file.copy(mathfile, paste0(dir_name,'/math_jax.html'))
  if(data){
    dir.create(paste0(dir_name,"/data"))
  }
  invisible(rstudioapi::documentOpen(paste0(dir_name,'/presentation.qmd')))
}

