#' Colourbox guide
#'
#' @export
guide_colourbox <- function(

  # title
  title = waiver(),
  title1.position = "top",
  title.theme = NULL,
  title.hjust = NULL,
  title.vjust = NULL,

  # bar
  barwidth = NULL,
  barheight = NULL,
  nbin = 100,

  # general
  reverse = FALSE,
  order = 0,
  available_aes = c("colour", "color", "fill"),

  ...) {

  if (!is.null(barwidth) && !is.unit(barwidth)) barwidth <- unit(barwidth, default.unit)
  if (!is.null(barheight) && !is.unit(barheight)) barheight <- unit(barheight, default.unit)

  structure(list(
    # title
    title = title,
    title1.position = title1.position,
    title.theme = title.theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,

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
  grob.box <- rasterGrob(
    image = image, width = boxwidth, height = boxheight, default.units = "cm",
    gp = gpar(col = NA), interpolate = FALSE
  )

  # titles

  # obtain the theme for the legend title. We need this both for the title grob
  # and to obtain the title fontsize.
  title.theme <- guide$title.theme %||% calc_element("legend.title", theme)

  title.hjust <- guide$title.hjust %||% theme$legend.title.align %||% title.theme$hjust %||% 0
  title.vjust <- guide$title.vjust %||% title.theme$vjust %||% 0.5

  grob.title1 <- ggname(
    "guide.title1",
    element_grob(
      title.theme,
      label = guide$title[1],
      hjust = title.hjust,
      vjust = title.vjust,
      margin_x = TRUE,
      margin_y = TRUE
    )
  )

  grob.title2 <- ggname(
    "guide.title2",
    element_grob(
      title.theme,
      label = guide$title[2],
      hjust = title.hjust,
      vjust = title.vjust,
      margin_x = TRUE,
      margin_y = TRUE
    )
  )


  title_width <- width_cm(grob.title1)
  title_height <- height_cm(grob.title1)
  title_fontsize <- title.theme$size %||% calc_element("legend.title", theme)$size %||% 0

  # gap between keys etc
  # the default horizontal and vertical gap need to be the same to avoid strange
  # effects for certain guide layouts
  hgap <- width_cm(theme$legend.spacing.x  %||% (0.5 * unit(title_fontsize, "pt")))
  vgap <- height_cm(theme$legend.spacing.y %||% (0.5 * unit(title_fontsize, "pt")))

  # box and label widths/heights
  bl_widths <- c(boxwidth)
  bl_heights <- c(boxheight)

  vps <- list(box.row = 1, box.col = 1, label.row = 0, label.col = 0)

  if (guide$title1.position == "top") {
    widths <- c(bl_widths, max(0, title_width - sum(bl_widths)))
    heights <- c(title_height, vgap, bl_heights)
    vps <- with(
      vps,
      list(
        box.row = box.row + 2, box.col = box.col,
        label.row = label.row + 2, label.col = label.col,
        title1.row = 1, title1.col = 1:length(widths)
      )
    )
  } else {
    widths <- c(bl_widths, max(0, title_width - sum(bl_widths)))
    heights <- c(bl_heights, vgap, title_height)
    vps <- with(
      vps,
      list(
        box.row = box.row, box.col = box.col,
        label.row = label.row, label.col = label.col,
        title1.row = length(heights), title1.col = 1:length(widths)
      )
    )
  }


  # background
  grob.background <- element_render(theme, "legend.background")
  # padding
  padding <- convertUnit(theme$legend.margin %||% margin(), "cm")
  widths <- c(padding[4], widths, padding[2])
  heights <- c(padding[1], heights, padding[3])

  gt <- gtable(widths = unit(widths, "cm"), heights = unit(heights, "cm"))
  gt <- gtable_add_grob(
    gt, grob.background, name = "background", clip = "off",
    t = 1, r = -1, b = -1, l = 1
  )
  gt <- gtable_add_grob(
    gt, grob.box, name = "box", clip = "off",
    t = 1 + min(vps$box.row), r = 1 + max(vps$box.col),
    b = 1 + max(vps$box.row), l = 1 + min(vps$box.col)
  )
  gt <- gtable_add_grob(
    gt,
    justify_grobs(
      grob.title1,
      hjust = title.hjust,
      vjust = title.vjust,
      int_angle = title.theme$angle,
      debug = title.theme$debug
    ),
    name = "title", clip = "off",
    t = 1 + min(vps$title1.row), r = 1 + max(vps$title1.col),
    b = 1 + max(vps$title1.row), l = 1 + min(vps$title1.col)
  )

  gt
}

#' @export
#' @rdname guide_colourbox
guide_colorbox <- guide_colourbox
