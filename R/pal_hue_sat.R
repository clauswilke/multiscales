#' Hue-saturation palette using HSV color space
#'
#' Returns a palette function that turns `h` and `s` values (both between 0 and 1) into
#' colors.
#' @param h_range The range of H values to be used.
#' @param s_range The range of S values to be used.
#' @param v The value (V) of the colors in HSV space.
#' @export
pal_hue_sat <- function(h_range = c(0, 1), s_range = c(0, 1), v = 1) {
  function(h, s){
    # hsv can't handle NAs, so we need to take care of that

    # first we find some value that is in the range of data
    hmin <- min(h, na.rm = TRUE)
    if (is.na(hmin)) hmin <- 0
    smin <- min(s, na.rm = TRUE)
    if (is.na(smin)) smin <- 0

    # now we find the NA values
    i1 <- is.na(h)
    i2 <- is.na(s)

    # and replace with the values previously identified
    h[i1] <- hmin
    s[i2] <- smin

    # now rescale to allowed limits
    h <- rescale(h, to = h_range)
    s <- rescale(s, to = s_range)

    # create colors
    ifelse(i1 | i2, NA, hsv(h, s, v))
  }
}
