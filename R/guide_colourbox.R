#' Colourbox guide
#'
#' @export
guide_colourbox <- function(

  # title
  title = waiver(),
  title.x.position = "top",
  title.y.position = "right",
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
    title.x.position = title.x.position,
    title.y.position = title.y.position,
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
  guide$box <- expand.grid(x = v1, y = v2)
  guide$box$colour <- scale$map(zip(guide$box$x, guide$box$y))

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
  title.x.position <- guide$title.x.position %||% "top"
  title.y.position <- guide$title.y.position %||% "right"


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

  # make title grobs if needed
  title.x.label <- guide$title[1]
  if (is.null(title.x.label) || is.na(title.x.label)) {
    title.x.position <- "none"
  } else {
    grob.title.x <- ggname(
      "guide.title.x",
      element_grob(
        title.theme,
        label = title.x.label,
        hjust = title.hjust,
        vjust = title.vjust,
        margin_x = TRUE,
        margin_y = TRUE
      )
    )
    title.x.width <- width_cm(grob.title.x)
    title.x.height <- height_cm(grob.title.x)
  }

  title.y.label <- guide$title[2]
  if (is.null(title.y.label) || is.na(title.y.label)) {
    title.y.position <- "none"
  } else {
    grob.title.y <- ggname(
      "guide.title.y",
      element_grob(
        title.theme,
        label = title.y.label,
        hjust = title.hjust,
        vjust = title.vjust,
        angle = -90, # angle hard-coded for now, needs to be fixed eventually, also further down in `justify_grobs()`
        margin_x = TRUE,
        margin_y = TRUE
      )
    )
    title.y.width <- width_cm(grob.title.y)
    title.y.height <- height_cm(grob.title.y)
  }

  # gap between keys etc
  # the default horizontal and vertical gap need to be the same to avoid strange
  # effects for certain guide layouts
  title_fontsize <- title.theme$size %||% calc_element("legend.title", theme)$size %||% 0
  hgap <- width_cm(theme$legend.spacing.x  %||% (0.5 * unit(title_fontsize, "pt")))
  vgap <- height_cm(theme$legend.spacing.y %||% (0.5 * unit(title_fontsize, "pt")))

  # legend padding
  padding <- convertUnit(theme$legend.margin %||% margin(), "cm")

  # we set up the entire legend as an 11x11 table which contains:
  # margin, title, gap, labels, ticks, box, ticks, labels, gap, title, margin
  # depending on where titles and labels are added, some cells remain empty

  widths <- c(padding[4], 0, 0, 0, 0, boxwidth, 0, 0, 0, 0, padding[2])
  heights <- c(padding[1], 0, 0, 0, 0, boxheight, 0, 0, 0, 0, padding[3])

  # titles
  grob.title.x.top <- NULL
  grob.title.x.bottom <- NULL
  if (title.x.position %in% c("top", "both")) {
    heights[2] <- title.x.height
    heights[3] <- vgap
    grob.title.x.top <- justify_grobs(
      grob.title.x,
      hjust = title.hjust,
      vjust = title.vjust,
      int_angle = title.theme$angle,
      debug = title.theme$debug
    )
  }
  if (title.x.position %in% c("bottom", "both")) {
    heights[10] <- title.x.height
    heights[9] <- vgap
    grob.title.x.bottom <- justify_grobs(
      grob.title.x,
      hjust = title.hjust,
      vjust = title.vjust,
      int_angle = title.theme$angle,
      debug = title.theme$debug
    )
  }

  grob.title.y.left <- NULL
  grob.title.y.right <- NULL
  if (title.y.position %in% c("left", "both")) {
    widths[2] <- title.y.width
    widths[3] <- hgap
    grob.title.y.left <- justify_grobs(
      grob.title.y,
      hjust = title.hjust,
      vjust = title.vjust,
      int_angle = -90,
      debug = title.theme$debug
    )
  }
  if (title.y.position %in% c("right", "both")) {
    widths[10] <- title.y.width
    widths[9] <- hgap
    grob.title.y.right <- justify_grobs(
      grob.title.y,
      hjust = title.hjust,
      vjust = title.vjust,
      int_angle = -90,
      debug = title.theme$debug
    )
  }

  # background
  grob.background <- element_render(theme, "legend.background")

  gt <- gtable(widths = unit(widths, "cm"), heights = unit(heights, "cm"))
  gt <- gtable_add_grob(
    gt, grob.background, name = "background", clip = "off",
    t = 1, r = -1, b = -1, l = 1
  )
  gt <- gtable_add_grob(
    gt, grob.box, name = "box", clip = "off",
    t = 6, r = 6, b = 6, l = 6
  )
  if (!is.null(grob.title.x.top)) {
    gt <- gtable_add_grob(
      gt, grob.title.x.top, name = "title.x", clip = "off",
      t = 2, r = 6, b = 2, l = 6
    )
  }
  if (!is.null(grob.title.x.bottom)) {
    gt <- gtable_add_grob(
      gt, grob.title.x.bottom, name = "title.x", clip = "off",
      t = 10, r = 6, b = 10, l = 6
    )
  }
  if (!is.null(grob.title.y.left)) {
    gt <- gtable_add_grob(
      gt, grob.title.y.left, name = "title.y", clip = "off",
      t = 6, r = 2, b = 6, l = 2
    )
  }
  if (!is.null(grob.title.y.right)) {
    gt <- gtable_add_grob(
      gt, grob.title.y.right, name = "title.y", clip = "off",
      t = 6, r = 10, b = 6, l = 10
    )
  }

  gt
}

#' @export
#' @rdname guide_colourbox
guide_colorbox <- guide_colourbox
