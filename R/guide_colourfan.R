#' Colourfan guide
#'
#' @export
guide_colourfan <- function(

  # title
  title = waiver(),
  title.x.position = "top",
  title.y.position = "right",
  title.theme = NULL,
  title.hjust = NULL, ## can be deleted?
  title.vjust = NULL, ## can be deleted?

  # label
  label = TRUE,
  label.theme = NULL,

  # bar
  barwidth = NULL,
  barheight = NULL,
  nbin = 32,

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

    # label
    label = label,
    label.theme = label.theme,

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
    name = "colourfan"),
    class = c("guide", "colourfan")
  )
}

#' @export
guide_train.colourfan <- function(guide, scale, aesthetic = NULL) {

  # do nothing if scale are inappropriate
  if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
    warning("colorfan guide needs appropriate scales: ",
            paste(guide$available_aes, collapse = ", "))
    return(NULL)
  }
  if (!scale$is_bivariate()) {
    warning("colorfan guide needs bivariate scales.")
    return(NULL)
  }

  # create tick positions and labels
  breaks <- scale$get_breaks()
  if (length(breaks[[1]]) == 0 && length(breaks[[2]]) == 0 ||
      all(is.na(breaks[[1]])) && all(is.na(breaks[[2]])))
    return()
  labels <- scale$get_labels(breaks)

  guide$ticks1 <- tibble(value = breaks[[1]], label = labels[[1]])
  guide$ticks2 <- tibble(value = breaks[[2]], label = labels[[2]])

  # needed to make guide show, even if this is not how we keep track of labels and ticks
  key <- as.data.frame(
    setNames(list(NA), aesthetic %||% scale$aesthetics[1]),
    stringsAsFactors = FALSE
  )
  guide$key <- key

  # fan specification
  limits <- scale$get_limits()
  v1 <- seq(limits[[1]][1], limits[[1]][2], length = guide$nbin)
  if (length(v1) == 0) {
    v1 = unique(limits[[1]])
  }
  v2 <- seq(limits[[2]][1], limits[[2]][2], length = guide$nbin)
  if (length(v2) == 0) {
    v2 = unique(limits[[2]])
  }
  # fan data matrix
  guide$fan <- expand.grid(x = v1, y = v2)
  guide$fan$colour <- scale$map(zip(guide$fan$x, guide$fan$y))

  # keep track of individual values along x and y also
  guide$fan.x <- v1
  guide$fan.y <- v2

  ## need to think about proper implementation
  #if (guide$reverse) {
  #  guide$key <- guide$key[nrow(guide$key):1, ]
  #  guide$bar <- guide$bar[nrow(guide$bar):1, ]
  #}
  guide$hash <- with(guide, digest::digest(list(title, ticks1, ticks2, name)))
  guide
}

# simply discards the new guide
#' @export
guide_merge.colourfan <- function(guide, new_guide) {
  guide
}

# this guide is not geom-based.
#' @export
guide_geom.colourfan <- function(guide, layers, default_mapping) {
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
guide_gengrob.colourfan <- function(guide, theme) {
  title.x.position <- guide$title.x.position %||% "top"
  title.y.position <- guide$title.y.position %||% "right"

  fanwidth <- width_cm(theme$legend.key.width * 5)
  fanheight <- height_cm(theme$legend.key.height * 5)
  nbreak <- nrow(guide$key)

  # make the fan grob (`grob.fan`)
  grob.fan <- colourfan_grob(guide$fan$colour, nrow = guide$nbin, ncol = guide$nbin)

  # make ticks and labels
  tick.x.pos <- rescale(
    guide$ticks1$value,
    c(0.5, guide$nbin - 0.5),
    guide$fan.x[c(1, length(guide$fan.x))]
  ) * fanwidth / guide$nbin
  label.x.pos <- unit(tick.x.pos, "cm")

  tick.y.pos <- rescale(
    guide$ticks2$value,
    c(guide$nbin - 0.5, 0.5),
    guide$fan.y[c(1, length(guide$fan.y))]
  ) * fanheight / guide$nbin
  label.y.pos <- unit(tick.y.pos, "cm")

  # make the label grobs (`grob.label.x` and `grob.label.y`)

  # get the label theme
  label.theme <- guide$label.theme %||% calc_element("legend.text", theme)

  # We break inheritance for hjust and vjust, because that's more intuitive here; it still allows manual
  # setting of hjust and vjust if desired. The alternative is to ignore hjust and vjust altogether, which
  # seems worse
  if (is.null(guide$label.theme$hjust) && is.null(theme$legend.text$hjust)) label.theme$hjust <- NULL
  if (is.null(guide$label.theme$vjust) && is.null(theme$legend.text$vjust)) label.theme$vjust <- NULL

  # label.theme in param of guide_legend() > theme$legend.text.align > default
  hjust <- label.theme$hjust %||% 0.5
  vjust <- label.theme$vjust %||% 0.5

  if (!guide$label) # are we drawing labels?
    grob.label.x <- NULL
  else {
    x <- label.x.pos
    y <- rep(vjust, length(label.x.pos))
    margin_x <- FALSE
    margin_y <- TRUE

    label <- guide$ticks1$label

    # If any of the labels are quoted language objects, convert them
    # to expressions. Labels from formatter functions can return these
    ## TODO: this should be a separate function to keep the code clean
    # maybe scales::parse_format()?
    if (any(vapply(label, is.call, logical(1)))) {
      label <- lapply(
        label,
        function(l) {
          if (is.call(l)) substitute(expression(x), list(x = l))
          else l
        }
      )
      label <- do.call(c, label)
    }
    grob.label.x <- element_grob(
      element = label.theme,
      label = label,
      x = x,
      y = y,
      hjust = hjust,
      vjust = vjust,
      margin_x = margin_x,
      margin_y = margin_y
    )
    grob.label.x <- ggname("guide.label.x", grob.label.x)
  }

  label.x.width <- width_cm(grob.label.x)
  label.x.height <- height_cm(grob.label.x)

  if (!guide$label) # are we drawing labels?
    grob.label.y <- NULL
  else {
    x <- rep(hjust, length(label.y.pos))
    y <- label.y.pos
    margin_x <- TRUE
    margin_y <- FALSE

    label <- guide$ticks2$label

    # If any of the labels are quoted language objects, convert them
    # to expressions. Labels from formatter functions can return these
    ## TODO: this should be a separate function to keep the code clean
    # maybe scales::parse_format()?
    if (any(vapply(label, is.call, logical(1)))) {
      label <- lapply(
        label,
        function(l) {
          if (is.call(l)) substitute(expression(x), list(x = l))
          else l
        }
      )
      label <- do.call(c, label)
    }
    grob.label.y <- element_grob(
      element = label.theme,
      label = label,
      x = x,
      y = y,
      hjust = hjust,
      vjust = vjust,
      margin_x = margin_x,
      margin_y = margin_y
    )
    grob.label.y <- ggname("guide.label.y", grob.label.y)
  }

  label.y.width <- width_cm(grob.label.y)
  label.y.height <- height_cm(grob.label.y)

  # make titles

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
  # margin, title, gap, labels, ticks, fan, ticks, labels, gap, title, margin
  # depending on where titles and labels are added, some cells remain empty

  widths <- c(padding[4], 0, 0, 0, 0, fanwidth, 0, 0, 0, 0, padding[2])
  heights <- c(padding[1], 0, 0, 0, 0, fanheight, 0, 0, 0, 0, padding[3])

  ## TODO: need to figure out where and how to correctly set label sizes
  heights[4] <- label.x.height
  widths[8] <- label.y.width

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
    gt, grob.fan, name = "fan", clip = "off",
    t = 6, r = 6, b = 6, l = 6
  )
  if (!is.null(grob.title.x.top)) {
    gt <- gtable_add_grob(
      gt, grob.title.x.top, name = "title.x.top", clip = "off",
      t = 2, r = 6, b = 2, l = 6
    )
  }
  if (!is.null(grob.label.x)) {
    gt <- gtable_add_grob(
      gt, grob.label.x, name = "label.x.top", clip = "off",
      t = 4, r = 6, b = 4, l = 6
    )
  }
  if (!is.null(grob.title.x.bottom)) {
    gt <- gtable_add_grob(
      gt, grob.title.x.bottom, name = "title.x.bottom", clip = "off",
      t = 10, r = 6, b = 10, l = 6
    )
  }
  if (!is.null(grob.title.y.left)) {
    gt <- gtable_add_grob(
      gt, grob.title.y.left, name = "title.y.left", clip = "off",
      t = 6, r = 2, b = 6, l = 2
    )
  }
  if (!is.null(grob.title.y.right)) {
    gt <- gtable_add_grob(
      gt, grob.title.y.right, name = "title.y.right", clip = "off",
      t = 6, r = 10, b = 6, l = 10
    )
  }
  if (!is.null(grob.label.y)) {
    gt <- gtable_add_grob(
      gt, grob.label.y, name = "label.y.top", clip = "off",
      t = 6, r = 8, b = 6, l = 8
    )
  }


  gt
}

#' @export
#' @rdname guide_colourfan
guide_colorfan <- guide_colourfan


colourfan_grob <- function(colours, nrow, ncol, nmunch = 10) {
  # the trick is that we first make square polygons and then transform coordinates
  dx <- 1 / ncol
  dy <- 1 / nrow

  # grid of base points
  x <- rep((0:(ncol-1))/ncol, nrow)
  y <- rep(((nrow-1):0)/nrow, each = ncol)

  # turn into polygon boundaries
  x <- unlist(lapply(x, function(x) c(x+dx*(0:nmunch)/nmunch, x+dx*(nmunch:0)/nmunch)))
  y <- unlist(lapply(y, function(y) c(rep(y, nmunch + 1), rep(y+dy, nmunch + 1))))
  id <- rep(1:(nrow*ncol), each = 22)

  # now transform coordinates and make polygon
  phi <- (x * 60 - 30)*(pi/180)
  r <- y
  Y <- r * cos(phi)
  X <- r * sin(phi) + 0.5
  polygonGrob(X, Y, id, gp = gpar(fill = colours, col = colours, lwd = 0.5, lty = 1))
}


