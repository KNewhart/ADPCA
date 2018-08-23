#' Install and load libraries
#'
#' This function checks to see if a package is already installed before installing and loading it.
#' @param packName package name as a character string, e.g. "quantmod"
#' @examples
#' packageLoad(ADPCA)
#' @export

packageLoad <- function(packName){ #packName - package name as a character string, e.g. "quantmod"
  if(!require(packName,character.only = TRUE)){ #If the package is not available, install it
    install.packages(packName,dependencies=TRUE,repos="http://cran.r-project.org")
  }
  library(packName, character.only = TRUE) # load package
}
