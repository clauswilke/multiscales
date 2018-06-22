
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

Define a multivariate palette function:

``` r
pal_hsv <- function(v = 1) {
  function(h, s){
    hsv(h, s, v)
  }
}
```

Make multivariate color
plot:

``` r
d <- expand.grid(x = seq(0, 1, length.out = 50), y = seq(0, 1, length.out = 50))

ggplot(d, aes(x, y, fill = zip(x, y))) +
  geom_tile() +
  bivariate_scale("fill", "bivariate_scale", pal_hsv(0.7))
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->