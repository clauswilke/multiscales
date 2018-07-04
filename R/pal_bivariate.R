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
pal_bivariate_carto <- function(palette = "Earth", max_light = 0.9, max_desat = 0, pow_light = 0.5, pow_desat = 1, ...) {
  cols <- colorspace::carto_hcl(n = 11, palette = palette, ...)
  ramp <- colour_ramp(cols)

  function(v, u){
    # limit maximal desaturation and lightening
    des_amt <- max_desat*u^pow_desat
    light_amt <- max_light*u^pow_light
    colorspace::lighten(colorspace::desaturate(ramp(v), des_amt), light_amt, space = "HLS")
  }
}

