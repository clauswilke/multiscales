#' @rdname bivariate_scale
#' @usage NULL
#' @export
ScaleBivariate <- ggproto("ScaleBivariate", Scale,
  range = bivariate_range(),
  #rescaler = ... # used by map() in ContinuousScale
  #oob =          # used by map() in ContinuousScale

  is_discrete = function() FALSE,

  train = function(self, x) {
    if (length(x) == 0) return()
    self$range$train(x)
  },

  transform = function(self, x) {
    new_x <- self$trans$transform(x)

    new_x
  },

  map = function(self, x, limits = self$get_limits()) {
    ## need to implement rescaling
    # x <- self$rescaler(self$oob(x, range = limits), from = limits)


    ## fix for data frames
    scaled <- self$palette(
      unlist(transpose(x)[[1]]),
      unlist(transpose(x)[[2]])
    )

    ifelse(!is.na(scaled), scaled, self$na.value)
  },

  #  if scale contains a NULL, use the default scale range
  #  if scale contains a NA, use the default range for that axis, otherwise
  #  use the user defined limit for that axis
  get_limits = function(self) {
    if (self$is_empty()) return(c(0, 1))

    if (!is.null(self$limits)) {
      ifelse(!is.na(self$limits), self$limits, self$range$range)
    } else {
      self$range$range
    }
  },

  get_breaks = function(self, limits = self$get_limits()) {
    NULL # not implemented
  },

  # The numeric position of scale breaks, used by coord/guide
  break_positions = function(self, range = self$get_limits()) {
    self$map(self$get_breaks(range))
  },

  get_breaks_minor = function(self, n = 2, b = self$break_positions(), limits = self$get_limits()) {
    NULL # not implemented
  },

  get_labels = function(self, breaks = self$get_breaks()) {
    NULL # not implemented
  },

  clone = function(self) {
    new <- ggproto(NULL, self)
    new$range <- bivariate_range()
    new
  },

  break_info = function(self, range = NULL) {
    NULL # not implemented
  }
)


#' Constructor for bivariate scale object
#'
#' @inheritParams ggplot2::continuous_scale
#' @export
bivariate_scale <- function(aesthetics, scale_name, palette, name = waiver(),
                            breaks = waiver(), minor_breaks = waiver(), labels = waiver(), limits = NULL,
                            rescaler = rescale, oob = censor, expand = waiver(), na.value = NA_real_,
                            trans = "identity", guide = "none", super = ScaleBivariate) {

  #check_breaks_labels(breaks, labels)

  #if (is.null(breaks) && !is_position_aes(aesthetics) && guide != "none") {
  #  guide <- "none"
  #}

  trans <- as.trans(trans)
  if (!is.null(limits)) {
    limits <- trans$transform(limits)
  }

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
    rescaler = rescaler,
    oob = oob,

    name = name,
    breaks = breaks,
    minor_breaks = minor_breaks,

    labels = labels,
    guide = guide
  )
}
