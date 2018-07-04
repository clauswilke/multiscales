#' Variance suppressing uncertainty palette
#'
#' Returns a palette function that turns `v` (value) and `u` (uncertainty) (both between 0 and 1) into
#' colors.
#' @param values Color values to be used at minimum uncertainty. Needs to be a vector of
#'   length `2^unc_levels`.
#' @param unc_levels Number of discrete uncertainty levels. The number of discrete colors
#'   at each level doubles.
#' @param max_light Maximum amount of lightening
#' @param max_desat Maximum amount of desaturation
#' @param pow_light Power exponent of lightening
#' @param pow_desat Power exponent of desaturation
#' @seealso [`pal_vsup_carto()`], [`pal_vsup_viridis()`]
#' @export
pal_vsup <- function(values, unc_levels = 4, max_light = 0.9, max_desat = 0, pow_light = 0.8, pow_desat = 1) {
  n <- 2^(unc_levels - 1)
  if (length(values) != n) {
    stop(length(values), " colors are provided but ", n, " colors are needed for ", unc_levels, " uncertainty levels.", call. = FALSE)
  }

  ramp <- colour_ramp(values)

  # v = value, 0: small, 1: large
  # u = uncertainty, 0: completely certain, 1: completely uncertain
  map_to_discrete <- function(v, u) {
    j <- 1 + floor((1 - u) * unc_levels)
    j <- ifelse(j >= unc_levels, unc_levels, j)

    val_levels <- 2^(j-1) # total number of value levels at that uncertainty
    i <- 1 + floor(v * val_levels)
    i <- ifelse( i >= val_levels, val_levels, i)

    list(i = i, j = j, v = ((i - 0.5)/val_levels - 0.5/n)*n/(n - 1), u = 1 - (j - 1)/(unc_levels - 1))
  }

  function(v, u){
    x <- map_to_discrete(v, u)
    v <- x$v
    u <- x$u # need maximum lightening for 0 certainty

    # limit maximal desaturation and lightening
    des_amt <- max_desat*u^pow_desat
    light_amt <- max_light*u^pow_light
    cols_des <- colorspace::desaturate(ramp(v), des_amt)
    nas <- is.na(light_amt)
    light_amt[nas] <- 0
    ifelse(nas, NA, colorspace::lighten(cols_des, light_amt, space = "HLS"))
  }
}


#' Value-suppressing uncertainty palettes using carto colors
#'
#' @inheritParams pal_vsup
#' @param palette Name of the palette
#' @param ... Other arguments to be given to [`carto_hcl()`].
#' @seealso [`pal_vsup()`]
#' @export
pal_vsup_carto <- function(palette = "Earth", max_light = 0.9, max_desat = 0, pow_light = 0.5, pow_desat = 1, unc_levels = 4, ...) {
  n <- 2^(unc_levels - 1)

  values <- colorspace::carto_hcl(n = n, palette = palette, ...)

  pal_vsup(values, unc_levels, max_light, max_desat, pow_light, pow_desat)
}

#' Value-suppressing uncertainty palettes using viridis colors
#'
#' @inheritParams pal_vsup
#' @param option Palette to be used, as in [`viridisLite::viridis()`].
#' @param begin Hue in \[0, 1] at which the colormap begins.
#' @param end Hue in \[0, 1] at which the colormap ends.
#' @param direction If 1 (default), colors go from light to dark. If -1, colors
#'   go dark to light. (Note that this is reversed from standard viridis setup.)
#' @param alpha Alpha transparency of the colors, specified as a number in \[0, 1].
#'   (0 means transparent and 1 means opaque)
#' @param ... Other arguments to be given to [`pal_vsup()`].
#' @seealso [`pal_vsup()`]
#' @export
pal_vsup_viridis <- function(unc_levels = 4, option = "E", begin = 0.1, end = 0.7, direction = 1,
                             alpha = 1, ...) {
  n <- 2^(unc_levels - 1)

  # swap direction relative to viridis()
  direction <- -1*direction
  tmp <- begin
  begin <- 1 - end
  end <- 1 - tmp

  values <- viridisLite::viridis(
    n = n,
    option = option,
    begin = begin,
    end = end,
    direction = direction,
    alpha = alpha
  )

  pal_vsup(values, unc_levels, ...)
}

