#' Median house values Florida counties
#'
#' Median house values in Florida counties, from the 2015 five-year American Community Survey.
#'
#' @examples
#' library(ggplot2)
#' library(colorspace)
#'
#' # B25077_001: Median house value in the past 12 months (in 2015 Inflation-adjusted dollars)
#'
#' # univariate scale
#' ggplot(FL_house_values, aes(fill = estimate)) +
#'   geom_sf(color = "gray30", size = 0.2) +
#'   coord_sf(xlim = c(-88, -79.8), ylim = c(24.1, 31.2), datum = NA) +
#'   scale_fill_continuous_carto(
#'     palette = "Sunset", rev = TRUE,
#'     name = "median house values",
#'     guide = guide_colorbar(
#'       direction = "horizontal",
#'       label.position = "bottom",
#'       title.position = "top",
#'       barwidth = grid::unit(2.0, "in")
#'     )
#'   ) +
#'   theme_void() +
#'   theme(
#'     legend.title.align = 0.5,
#'     legend.text.align = 0.5,
#'     legend.justification = c(0, 0),
#'     legend.position = c(0.1, 0.3)
#'   )
#'
#' # bivariate value-suppressing uncertainty scale
#' ggplot(FL_house_values, aes(fill = zip(estimate, moe/estimate))) +
#'   geom_sf(color = "gray30", size = 0.2) +
#'   coord_sf(xlim = c(-88, -79.8), ylim = c(24.1, 31.2), datum = NA) +
#'   bivariate_scale(
#'     "fill", "bivariate_scale",
#'     pal_carto_vsup(palette = "Sunset", rev = TRUE),
#'     guide = "colourbox",
#'     name = c("median house values", "uncertainty")
#'   ) +
#'   theme_void() +
#'   theme(
#'     legend.title.align = 0.5,
#'     legend.text.align = 0.5,
#'     legend.justification = c(0, 0),
#'     legend.position = c(0.15, 0.2)
#'   )
#'
"FL_house_values"

#' Polling data from the 2016 US presidential election
#'
#' Polling data from the 2016 US presidential election, combined with map of US states. Also provided is an
#' alternative map in cartogram style where each state is scaled in proportion to the number of electoral
#' college votes it has.
#'
#' @source
#' Michael Correll, Dominik Moritz, Jeffrey Heer (2018) Value-Suppressing Uncertainty Palettes.
#' ACM Human Factors in Computing Systems (CHI)
#' \url{https://github.com/uwdata/papers-vsup/tree/master/examples}
"US_polling"

#' @rdname US_polling
"US_polling_cartogram"
