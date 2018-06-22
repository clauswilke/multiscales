#' @rdname bivariate_scale
#' @usage NULL
#' @export
ScaleBivariate <- ggproto("ScaleBivariate", Scale,
  range = bivariate_range(),
  rescaler1 = rescale,
  rescaler2 = rescale,
  oob = censor,

  is_discrete = function() FALSE,

  train = function(self, x) {
    if (length(x) == 0) return()
    self$range$train(x)
  },

  transform = function(self, x) {
    return(x) # transformation not yet implemented
    #new_x <- self$trans$transform(x)

    #new_x
  },

  map = function(self, x, limits = self$get_limits()) {
    ## fix for data frames
    x1 <- unlist(transpose(x)[[1]])
    x2 <- unlist(transpose(x)[[2]])

    x1 <- self$rescaler1(self$oob(x1, range = limits[[1]]), from = limits[[1]])
    x2 <- self$rescaler2(self$oob(x2, range = limits[[2]]), from = limits[[2]])

    scaled <- self$palette(x1, x2)

    ifelse(!is.na(scaled), scaled, self$na.value)
  },

  #  if scale contains a NULL, use the default scale range
  #  if scale contains a NA, use the default range for that axis, otherwise
  #  use the user defined limit for that axis
  get_limits = function(self) {
    if (self$is_empty()) return(tibble(limits1 = c(0, 1), limits2 = c(0, 1)))

    if (is.null(self$limits)) {
      return(tibble(limits1 = self$range$range[[1]], limits2 = self$range$range[[2]]))
    } else {
      limits1 <- ifelse(!is.na(self$limits[[1]]), self$limits[[1]], self$range$range[[1]])
      limits2 <- ifelse(!is.na(self$limits[[2]]), self$limits[[2]], self$range$range[[2]])
      return(tibble(limits1, limits2))
    }
  },

  get_breaks = function(self, limits = self$get_limits()) {
    NULL # not implemented
  },

  # The numeric position of scale breaks, used by coord/guide
  break_positions = function(self, range = self$get_limits()) {
    self$map(self$get_breaks(range))
  },

  get_labels = function(self, breaks = self$get_breaks()) {
    NULL # not implemented
  },

  clone = function(self) {
    new <- ggproto(NULL, self)
    new$range <- bivariate_range()
    new
  }
)


#' Constructor for bivariate scale object
#'
#' @inheritParams ggplot2::continuous_scale
#' @param limits Data frame with two columns of length two each defining the limits for the two data dimensions.
#' @param rescaler1 Rescaling function for the first data dimension.
#' @param rescaler2 Rescaling function for the second data dimension. By default, equal to `rescaler1`.
#' @export
bivariate_scale <- function(aesthetics, scale_name, palette, name = waiver(),
                            breaks = waiver(), minor_breaks = waiver(), labels = waiver(), limits = NULL,
                            rescaler1 = rescale, rescaler2 = rescaler1, oob = censor, expand = waiver(), na.value = NA_real_,
                            trans = "identity", guide = "none", super = ScaleBivariate) {

  #check_breaks_labels(breaks, labels)

  #if (is.null(breaks) && !is_position_aes(aesthetics) && guide != "none") {
  #  guide <- "none"
  #}
  guide <- "none" # guide doesn't work yet

  if (!is.null(limits)) {
    # Check that limits are data frame or list with two columns of two values
    if (!is.list(limits)) {
      stop("Limits argument has to be a data frame or list of vectors", call. = FALSE)
    } else if (length(limits) != 2 || length(limits[[1]]) != 2 || length(limits[[2]]) != 2) {
      stop("Limits need to be two values each for both data dimensions", call. = FALSE)
    }
  }

  #
  #trans <- as.trans(trans)
  #if (!is.null(limits)) {
  #  limits <- trans$transform(limits)
  #}

  ggproto(
    NULL, super,
    call = match.call(),

    aesthetics = aesthetics,
    scale_name = scale_name,
    palette = palette,

    #range = continuous_range(),
    limits = limits,
    trans = trans,
    na.value = na.value,
    expand = expand,
    rescaler1 = rescaler1,
    rescaler2 = rescaler2,
    oob = oob,

    name = name,
    breaks = breaks,
    minor_breaks = minor_breaks,

    labels = labels,
    guide = guide
  )
}
