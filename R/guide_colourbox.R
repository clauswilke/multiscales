#' Colourbox guide
#'
#' @export
guide_colourbox <- function(

  # bar
  barwidth = NULL,
  barheight = NULL,
  nbin = 20,

  # general
  reverse = FALSE,
  order = 0,
  available_aes = c("colour", "color", "fill"),

  ...) {

  if (!is.null(barwidth) && !is.unit(barwidth)) barwidth <- unit(barwidth, default.unit)
  if (!is.null(barheight) && !is.unit(barheight)) barheight <- unit(barheight, default.unit)

  structure(list(

    # bar
    barwidth = barwidth,
    barheight = barheight,
    nbin = nbin,

    # general
    reverse = reverse,
    order = order,

    # parameter
    available_aes = available_aes,
    ...,
    name = "colourbox"),
    class = c("guide", "colourbox")
  )
}

#' @export
guide_train.colourbox <- function(guide, scale, aesthetic = NULL) {

  # do nothing if scale are inappropriate
  if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
    warning("colorbox guide needs appropriate scales: ",
            paste(guide$available_aes, collapse = ", "))
    return(NULL)
  }
  if (!scale$is_bivariate()) {
    warning("colorbox guide needs bivariate scales.")
    return(NULL)
  }

  # create data frame for tick display
  ## Uncomment once breaks are properly implemented
  #breaks <- scale$get_breaks()
  #if (length(breaks) == 0 || all(is.na(breaks)))
  #  return()

  #ticks <- as.data.frame(setNames(list(scale$map(breaks)), aesthetic %||% scale$aesthetics[1]))
  #ticks$.value <- breaks
  #ticks$.label <- scale$get_labels(breaks)

  # needed to make guide show, even if this is not how we keep track of labels and ticks
  key <- as.data.frame(
    setNames(list(NA), aesthetic %||% scale$aesthetics[1]),
    stringsAsFactors = FALSE
  )
  guide$key <- key

  # box specification
  limits <- scale$get_limits()
  v1 <- seq(limits[[1]][1], limits[[1]][2], length = guide$nbin)
  if (length(v1) == 0) {
    v1 = unique(limits[[1]])
  }
  v2 <- seq(limits[[2]][1], limits[[2]][2], length = guide$nbin)
  if (length(v2) == 0) {
    v2 = unique(limits[[2]])
  }
  guide$box <- expand.grid(v1 = v1, v2 = v2)
  guide$box$colour <- scale$map(zip(guide$box$v1, guide$box$v2))

  ## need to think about proper implementation
  #if (guide$reverse) {
  #  guide$key <- guide$key[nrow(guide$key):1, ]
  #  guide$bar <- guide$bar[nrow(guide$bar):1, ]
  #}
  #guide$hash <- with(guide, digest::digest(list(title, key$.label, bar, name)))
  guide$hash <- digest::digest("colourbox") # temporary fix
  guide
}

# simply discards the new guide
#' @export
guide_merge.colourbox <- function(guide, new_guide) {
  guide
}

# this guide is not geom-based.
#' @export
guide_geom.colourbox <- function(guide, layers, default_mapping) {
  # Layers that use this guide
  guide_layers <- plyr::llply(layers, function(layer) {
    matched <- matched_aes(layer, guide, default_mapping)

    if (length(matched) && ((is.na(layer$show.legend) || layer$show.legend))) {
      layer
    } else {
      # This layer does not use this guide
      NULL
    }
  })

  # Remove this guide if no layer uses it
  if (length(plyr::compact(guide_layers)) == 0) guide <- NULL

  guide
}

#' @export
guide_gengrob.colourbox <- function(guide, theme) {
  boxwidth <- width_cm(theme$legend.key.width * 5)
  boxheight <- height_cm(theme$legend.key.height * 5)
  nbreak <- nrow(guide$key)

  # make the bar grob (`grob.bar`)
  image <- matrix(guide$box$colour, nrow = guide$nbin, ncol = guide$nbin, byrow = TRUE)
  grob.box <- rasterGrob(image = image, width = boxwidth, height = boxheight, default.units = "cm", gp = gpar(col = NA), interpolate = TRUE)

  # background
  grob.background <- ggplot2:::element_render(theme, "legend.background")
  # padding
  padding <- convertUnit(theme$legend.margin %||% margin(), "cm")
  widths <- c(padding[4], boxwidth, padding[2])
  heights <- c(padding[1], boxheight, padding[3])

  gt <- gtable(widths = unit(widths, "cm"), heights = unit(heights, "cm"))
  gt <- gtable_add_grob(gt, grob.background, name = "background", clip = "off",
                        t = 1, r = -1, b = -1, l = 1)
  gt <- gtable_add_grob(gt, grob.box, name = "box", clip = "off",
                        t = 1 + 1, r = 1 + 1,
                        b = 1 + 1, l = 1 + 1)

  gt
}

#' @export
#' @rdname guide_colourbox
guide_colorbox <- guide_colourbox
