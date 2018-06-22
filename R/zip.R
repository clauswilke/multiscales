#' Zip two or more lists into a list of lists
#'
#' @param ... Lists to be zipped
#' @export
zip <- function(...) transpose(list(...))
