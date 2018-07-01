#' @rdname bivariate_scale
#' @usage NULL
#' @export
ScaleBivariate <- ggproto("ScaleBivariate", Scale,
  range = bivariate_range(),
  rescaler = list(rescale, rescale),
  oob = censor,
  trans = list(identity_trans, identity_trans),

  is_discrete = function() FALSE,
  is_bivariate = function() TRUE,

  train = function(self, x) {
    if (length(x) == 0) return()
    self$range$train(x)
  },

  transform = function(self, x) {
    ## fix for data frames
    x1 <- unlist(transpose(x)[[1]])
    x2 <- unlist(transpose(x)[[2]])

    x1 <- self$trans[[1]]$transform(x1)
    x2 <- self$trans[[2]]$transform(x2)

    ## fix for data frames
    zip(x1, x2)
  },

  map = function(self, x, limits = self$get_limits()) {
    ## fix for data frames
    x1 <- unlist(transpose(x)[[1]])
    x2 <- unlist(transpose(x)[[2]])

    x1 <- self$rescaler[[1]](self$oob(x1, range = limits[[1]]), from = limits[[1]])
    x2 <- self$rescaler[[2]](self$oob(x2, range = limits[[2]]), from = limits[[2]])

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
    breaks1 <- self$get_breaks_1d(1, limits[[1]])
    breaks2 <- self$get_breaks_1d(2, limits[[2]])

    list(breaks1 = breaks1, breaks2 = breaks2)
  },

  # breaks for one data dimension
  get_breaks_1d = function(self, i = 1, limits = self$get_limits()[[i]]) {
    if (self$is_empty()) return(numeric(0))

    # Limits in transformed space need to be converted back to data space
    limits <- self$trans[[i]]$inverse(limits)

    if (is.null(self$breaks)) {
      return(NULL)
    } else if (identical(self$breaks[[i]], NA)) {
      stop("Invalid breaks specification. Use NULL, not NA")
    } else if (zero_range(as.numeric(limits))) {
      breaks <- limits[[i]][1]
    } else if (is.waive(self$breaks[[i]])) {
      breaks <- self$trans[[i]]$breaks(limits)
    } else if (is.function(self$breaks[[i]])) {
      breaks <- self$breaks[[i]](limits)
    } else {
      breaks <- self$breaks[[i]]
    }

    # Breaks in data space need to be converted back to transformed space
    # And any breaks outside the dimensions need to be flagged as missing
    #
    # @kohske
    # TODO: replace NA with something else for flag.
    #       guides cannot discriminate oob from missing value.
    breaks <- censor(self$trans[[i]]$transform(breaks), self$trans[[i]]$transform(limits),
                     only.finite = FALSE)
    breaks
  },

  get_labels = function(self, breaks = self$get_breaks()) {
    labels1 <- self$get_labels_1d(1, breaks[[1]])
    labels2 <- self$get_labels_1d(2, breaks[[2]])

    list(labels1 = labels1, labels2 = labels2)
  },

  # labels for one data dimension
  get_labels_1d = function(self, i = 1, breaks = self$get_breaks()[[i]]) {
    if (is.null(breaks)) return(NULL)

    breaks <- self$trans[[i]]$inverse(breaks)

    if (is.null(self$labels[[i]])) {
      return(NULL)
    } else if (identical(self$labels[[i]], NA)) {
      stop("Invalid labels specification. Use NULL, not NA", call. = FALSE)
    } else if (is.waive(self$labels[[i]])) {
      labels <- self$trans[[i]]$format(breaks)
    } else if (is.function(self$labels[[i]])) {
      labels <- self$labels[[i]](breaks)
    } else {
      labels <- self$labels[[i]]
    }
    if (length(labels) != length(breaks)) {
      stop("Breaks and labels are different lengths")
    }
    labels
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
#' @param trans Either one transformation applied to both data dimensions or list of two transformations, one
#'   for each data dimension. Transformations can be given as either the name of a transformation object
#'   or the object itself. See [`ggplot2::continuous_scale()`] for details.
#' @param rescaler Either one rescaling function applied to both data dimensions or list of two rescaling functions,
#'   one for each data dimension.
#' @export
bivariate_scale <- function(aesthetics, palette, name = waiver(),
                            breaks = waiver(), labels = waiver(), limits = NULL,
                            rescaler = rescale, oob = censor, expand = waiver(), na.value = NA_real_,
                            trans = "identity", guide = "none", super = ScaleBivariate,
                            scale_name = "bivariate_scale") {

  breaks <- bivariatize_arg(breaks, "breaks")
  labels <- bivariatize_arg(labels, "labels")

  #check_breaks_labels(breaks, labels)

  #if (is.null(breaks) && !is_position_aes(aesthetics) && guide != "none") {
  #  guide <- "none"
  #}

  # TODO
  # Need to bivariatize censor, oob, and expand
  trans <- bivariatize_arg(trans, "trans")
  trans[[1]] <- as.trans(trans[[1]])
  trans[[2]] <- as.trans(trans[[2]])

  rescaler <- bivariatize_arg(rescaler, "rescaler")

  if (!is.null(limits)) {
    # Check that limits are data frame or list with two columns of two values
    if (!is.list(limits)) {
      stop("Limits argument has to be a data frame or list of vectors", call. = FALSE)
    } else if (length(limits) != 2 || length(limits[[1]]) != 2 || length(limits[[2]]) != 2) {
      stop("Limits need to be two values each for both data dimensions", call. = FALSE)
    }

    # limits are given and valid, need to transform
    limits <- tibble(
      limits1 = trans[[1]]$transform(limits[[1]]),
      limits2 = trans[[2]]$transform(limits[[2]])
    )
  }

  ggproto(
    NULL, super,
    call = match.call(),

    aesthetics = aesthetics,
    scale_name = scale_name,
    palette = palette,

    range = bivariate_range(),
    limits = limits,
    trans = trans,
    na.value = na.value,
    expand = expand,
    rescaler = rescaler,
    oob = oob,

    name = name,
    breaks = breaks,

    labels = labels,
    guide = guide
  )
}

bivariatize_arg <- function(arg, name = "argument") {
  if (!is.null(oldClass(arg)) || is.function(arg) || is.atomic(arg)) {
    return(list(arg, arg))
  }

  if (!is.list(arg) || length(arg) != 2) {
    stop(paste0("In `bivariate_scale()`, argument `", name, "` needs to be given either as one argument applied to both data dimensions or as a list of exactly two arguments."), call. = FALSE)
  }

  arg
}
