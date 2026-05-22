# Helpers copied or adapted from ggplot2.
#
# Historically this package reached into ggplot2 internals via `:::`. Several of
# those internals were removed or renamed in the ggplot2 3.5.0 guide-system
# rewrite (and subsequent releases up to 4.0.x). To remain compatible across
# ggplot2 versions, the functions that no longer exist are vendored here, and
# the ones that still exist internally are accessed defensively.

width_cm <- function(x) {
  if (is.grob(x)) {
    convertWidth(grobWidth(x), "cm", TRUE)
  } else if (is.unit(x)) {
    convertWidth(x, "cm", TRUE)
  } else if (is.list(x)) {
    vapply(x, width_cm, numeric(1))
  } else {
    stop("Unknown input")
  }
}

height_cm <- function(x) {
  if (is.grob(x)) {
    convertHeight(grobHeight(x), "cm", TRUE)
  } else if (is.unit(x)) {
    convertHeight(x, "cm", TRUE)
  } else if (is.list(x)) {
    vapply(x, height_cm, numeric(1))
  } else {
    stop("Unknown input")
  }
}

matched_aes <- function(layer, guide, defaults = NULL) {
  # In ggplot2 >= 3.5.0 the (legacy) `GuideOld` shim calls `guide_geom()` with
  # `default_mapping = NULL`, so we can no longer rely on the plot's default
  # mapping being passed in. Instead we use `layer$computed_mapping`, which the
  # modern build process populates with the fully-resolved mapping (including
  # aesthetics inherited from the plot). We fall back to the old behaviour for
  # older ggplot2 versions that still provide `defaults`.
  mapping <- layer$computed_mapping %||% layer$mapping
  all <- names(c(mapping, if (isTRUE(layer$inherit.aes)) defaults, layer$stat$default_aes))

  geom <- c(layer$geom$required_aes, names(layer$geom$default_aes))
  matched <- intersect(intersect(all, geom), names(guide$key))

  geom_params <- layer$computed_geom_params %||% layer$geom_params
  matched <- setdiff(matched, names(geom_params))
  setdiff(matched, names(layer$aes_params))
}

# `ggname()` and `element_render()` still exist as ggplot2 internals across the
# 3.x and 4.x series. Access them defensively so a future rename degrades into a
# clear error rather than a load-time failure of the whole package.
ggname <- function(prefix, grob) {
  fn <- tryCatch(
    get("ggname", envir = asNamespace("ggplot2")),
    error = function(e) NULL
  )
  if (is.null(fn)) {
    # Minimal fallback mirroring ggplot2's historical implementation.
    grob$name <- grobName(grob, prefix)
    return(grob)
  }
  fn(prefix, grob)
}

element_render <- function(theme, element, ...) {
  fn <- tryCatch(
    get("element_render", envir = asNamespace("ggplot2")),
    error = function(e) NULL
  )
  if (is.null(fn)) {
    stop(
      "Could not find `ggplot2:::element_render()`. ",
      "This version of ggplot2 may not be supported by `multiscales`.",
      call. = FALSE
    )
  }
  fn(theme, element, ...)
}

# `is.waive()` was renamed to the (internal) `is_waiver()` in ggplot2 3.5.0.
# Vendor a self-contained version; the underlying "waiver" S3 class is stable.
is.waive <- function(x) {
  inherits(x, "waiver")
}

# `justify_grobs()` was removed from ggplot2 during the guide-system rewrite.
# Vendored verbatim from ggplot2 3.4.4 (R/margins.R), with its `rotate_just()`
# dependency also vendored below so this package no longer depends on ggplot2
# internals for guide rendering.
justify_grobs <- function(grobs, x = NULL, y = NULL, hjust = 0.5, vjust = 0.5,
                          int_angle = 0, debug = FALSE) {
  if (!inherits(grobs, "grob")) {
    if (is.list(grobs)) {
      return(lapply(grobs, justify_grobs, x, y, hjust, vjust, int_angle, debug))
    } else {
      stop("`grobs` must be an individual `grob` or list of `grob` objects.", call. = FALSE)
    }
  }

  if (inherits(grobs, "zeroGrob")) {
    return(grobs)
  }

  # adjust hjust and vjust according to internal angle
  just <- rotate_just(int_angle, hjust, vjust)

  x <- x %||% unit(just$hjust, "npc")
  y <- y %||% unit(just$vjust, "npc")

  if (isTRUE(debug)) {
    children <- gList(
      rectGrob(gp = gpar(fill = "lightcyan", col = NA)),
      grobs
    )
  } else {
    children <- gList(grobs)
  }

  result_grob <- gTree(
    children = children,
    vp = viewport(
      x = x,
      y = y,
      width = grobWidth(grobs),
      height = grobHeight(grobs),
      just = unlist(just)
    )
  )

  if (isTRUE(debug)) {
    grobTree(
      result_grob,
      pointsGrob(x, y, pch = 20, gp = gpar(col = "mediumturquoise"))
    )
  } else {
    result_grob
  }
}

# Vendored from ggplot2 3.4.4 (R/margins.R). Rotates justification parameters
# counter-clockwise for the supported right-angle cases.
rotate_just <- function(angle, hjust, vjust) {
  angle <- (angle %||% 0) %% 360
  if (0 <= angle & angle < 90) {
    hnew <- hjust
    vnew <- vjust
  } else if (90 <= angle & angle < 180) {
    hnew <- 1 - vjust
    vnew <- hjust
  } else if (180 <= angle & angle < 270) {
    hnew <- 1 - hjust
    vnew <- 1 - vjust
  } else if (270 <= angle & angle < 360) {
    hnew <- vjust
    vnew <- 1 - hjust
  }

  list(hjust = hnew, vjust = vnew)
}

# Replacement for `plyr::compact()`: drop NULL elements from a list.
compact <- function(l) {
  l[!vapply(l, is.null, logical(1))]
}

# Resolve a legend/theme element robustly across ggplot2 versions.
#
# In ggplot2 >= 3.5.0 many `legend.*` theme slots are not populated directly on
# the theme object handed to a guide's draw method; they must be resolved with
# `calc_element()` (which also applies inheritance, e.g. legend.key.width
# inheriting from legend.key.size). This helper resolves an element and falls
# back to `default` when resolution yields NULL.
calc_theme_element <- function(theme, element, default = NULL) {
  res <- tryCatch(calc_element(element, theme), error = function(e) NULL)
  res %||% theme[[element]] %||% default
}

# Resolve legend key width/height to a grid unit, with sensible fallbacks.
legend_key_dim <- function(theme, which = c("width", "height")) {
  which <- match.arg(which)
  el <- if (which == "width") "legend.key.width" else "legend.key.height"
  dim <- calc_theme_element(theme, el)
  if (is.null(dim)) {
    dim <- calc_theme_element(theme, "legend.key.size")
  }
  if (is.null(dim)) {
    # Final fallback matches ggplot2's historical default key size.
    dim <- unit(1.2, "lines")
  }
  dim
}
