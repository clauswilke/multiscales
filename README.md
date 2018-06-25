
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multiscales

Multivariate scales for ggplot2, written by Claus O. Wilke

## Installation

This package can be installed from github. It requires the development
version of the colorspace
    package.

    install.packages("colorspace", repos = "http://R-Forge.R-project.org")
    devtools::install_github("clauswilke/multiscales")

This is an experimental package. Use at your own risk. API is not
stable. No user support provided.

## Examples

Visualizing both the median house value and its uncertainty for Florida
counties:

``` r
library(ggplot2)
library(multiscales)
library(colorspace)

ggplot(FL_house_values, aes(fill = zip(estimate, moe/estimate))) +
  geom_sf(color = "gray30", size = 0.2) +
  coord_sf(xlim = c(-88, -79.8), ylim = c(24.1, 31.2), datum = NA) +
  bivariate_scale(
    "fill", "bivariate_scale",
    pal_carto_vsup(palette = "Sunset", rev = TRUE),
    guide = "colourbox",
    name = c("median house values", "uncertainty")
  ) +
  theme_void() +
  theme(
    legend.title.align = 0.5,
    legend.text.align = 0.5,
    legend.justification = c(0, 0),
    legend.position = c(0.15, 0.2)
  )
```

![](man/figures/README-unnamed-chunk-2-1.png)<!-- -->

For comparison, the same plot with a univariate color scale:

``` r
ggplot(FL_house_values, aes(fill = estimate)) +
  geom_sf(color = "gray30", size = 0.2) +
  coord_sf(xlim = c(-88, -79.8), ylim = c(24.1, 31.2), datum = NA) +
  scale_fill_continuous_carto(
    palette = "Sunset", rev = TRUE,
    name = "median house values",
    guide = guide_colorbar(
      direction = "horizontal",
      label.position = "bottom",
      title.position = "top",
      barwidth = grid::unit(2.0, "in")
    )
  ) +
  theme_void() +
  theme(
    legend.title.align = 0.5,
    legend.text.align = 0.5,
    legend.justification = c(0, 0),
    legend.position = c(0.1, 0.3)
  )
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->
