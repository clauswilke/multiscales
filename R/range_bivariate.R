#' Train range for bivariate scale
#'
#' @param new New data on which to train.
#' @param existing Existing range
#' @export
train_bivariate <- function(new, existing = NULL) {
  if (is.null(new)) return(existing)

  ## fix for data frames
  range1 <- scales::train_continuous(unlist(transpose(new)[[1]]), existing$range1)
  range2 <- scales::train_continuous(unlist(transpose(new)[[2]]), existing$range2)

  tibble(range1, range2)
}


Range <- ggproto("Range", NULL,
  range = NULL,
  reset = function(self) {
    self$range <- NULL
  }
)

#' @rdname bivariate_range
#' @usage NULL
#' @export
RangeBivariate <- ggproto("RangeBivariate", Range,
  train = function(self, x) {
    self$range <- train_bivariate(x, self$range)
  }
)

#' Constructor for bivariate range object
#' @export
bivariate_range <- function() {
  ggproto(NULL, RangeBivariate)
}
