# copied over from ggplot2

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

matched_aes <- function(layer, guide, defaults) {
  all <- names(c(layer$mapping, if (layer$inherit.aes) defaults, layer$stat$default_aes))
  geom <- c(layer$geom$required_aes, names(layer$geom$default_aes))
  matched <- intersect(intersect(all, geom), names(guide$key))
  matched <- setdiff(matched, names(layer$geom_params))
  setdiff(matched, names(layer$aes_params))
}

# not copied for now
element_render <- ggplot2:::element_render
ggname <- ggplot2:::ggname
justify_grobs <- ggplot2:::justify_grobs
