
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multiscales

Investigating multivariate scales for ggplot2, written by Claus O. Wilke

## Installation

    devtools::install_github("clauswilke/multiscales")

This is an experimental package. It may not work at all. Use at your own
risk. API is not stable. No user support provided.

## Examples

Load required packages:

``` r
library(ggplot2)
library(multiscales)
```

Make multivariate color plot:

``` r
d <- expand.grid(x = 1:100, y = 1:100)

ggplot(d, aes(x, y, fill = zip(x, y))) +
  geom_tile() +
  bivariate_scale(
    "fill", "bivariate_scale",
    palette = pal_hue_sat(h_range = c(0.2, 0.8), s_range = c(0, .6), v = 0.7)
  )
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->
