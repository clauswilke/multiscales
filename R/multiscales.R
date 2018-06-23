#' Multivariate scales for ggplot2
#'
#' @name multiscales
#' @docType package
#' @import ggplot2
#' @import gtable
#' @import grid
#' @import rlang
#' @import scales
#' @import tibble
#' @importFrom purrr transpose
NULL


# *************************************************
#                     Setup
# *************************************************

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Note: The package \"multiscales\" is highly experimental. It may not work at all. Use at your own risk.")
}
