#' Multivariate scales for ggplot2
#'
#' @name multiscales
#' @docType package
#' @import scales
#' @import tibble
#' @importFrom purrr transpose
#' @import ggplot2
NULL


# *************************************************
#                     Setup
# *************************************************

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Note: The package \"multiscales\" is highly experimental. It may not work at all. Use at your own risk.")
}
