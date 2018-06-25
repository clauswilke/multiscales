#' Bivariate palette based on Carto colors
#'
#' Returns a palette function that turns `v` (value) and `u` (uncertainty) (both between 0 and 1) into
#' colors.
#' @param palette Name of the palette
#' @param max_light Maximum amount of lightening
#' @param max_desat Maximum amount of desaturation
#' @param pow_light Power exponent of lightening
#' @param pow_desat Power exponent of desaturation
#' @param ... Other arguments to be given to `carto_hcl()`
#' @export
pal_carto_bivariate <- function(palette = "Earth", max_light = 0.9, max_desat = 0.9, pow_light = 0.5, pow_desat = 1, ...) {
  cols <- colorspace::carto_hcl(n = 11, palette = palette, ...)
  ramp <- colour_ramp(cols)

  function(v, u){
    # limit maximal desaturation and lightening
    des_amt <- max_desat*u^pow_desat
    light_amt <- max_light*u^pow_light
    colorspace::lighten(colorspace::desaturate(ramp(v), des_amt), light_amt)
  }
}

#' @rdname pal_carto_bivariate
#' @param unc_levels Number of discrete uncertainty levels. The number of discrete colors at each level doubles.
#' @export
pal_carto_vsup <- function(palette = "Earth", max_light = 0.9, max_desat = 0.9, pow_light = 0.5, pow_desat = 1, unc_levels = 4, ...) {
  n <- 2^(unc_levels - 1)

  cols <- colorspace::carto_hcl(n = n, palette = palette, ...)

  map_to_discrete <- function(v, u) {
    u <- 1 - u # u = 0 means maximum certainty
    j <- 1 + floor(u * unc_levels)
    j <- ifelse(j >= unc_levels, unc_levels, j)

    val_levels <- 2^(j-1) # total number of value levels at that uncertainty
    i <- 1 + floor(v * val_levels)
    i <- ifelse( i >= val_levels, val_levels, i)

    list(i = i, j = j, v = ((i - 0.5)/val_levels - 0.5/n)*n/(n - 1), u = (1 - j/unc_levels)/(1 - 1/unc_levels))
  }

  ramp <- colour_ramp(cols)

  function(v, u){
    x <- map_to_discrete(v, u)
    v <- x$v
    u <- x$u

    # limit maximal desaturation and lightening
    des_amt <- max_desat*u^pow_desat
    light_amt <- max_light*u^pow_light
    cols_des <- colorspace::desaturate(ramp(v), des_amt)
    nas <- is.na(light_amt)
    light_amt[nas] <- 0
    ifelse(nas, NA, colorspace::lighten(cols_des, light_amt))
  }
}
