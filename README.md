
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quaketools

<!-- badges: start -->
<!-- badges: end -->

The goal of quaketools is to provide a collection of tools to make
earthquake modelling easier. Functions will be provided to simulate
from, estimate and evaluate common models used in statistical
seismology.

## Installation

You can install the development version of quaketools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("zakvarty/quaketools")
```

## Example

This is a basic example which shows you how to simulate random variates
from a generalised Pareto distribution using `rgpd()`.

``` r
library(quaketools)
rgpd(n = 10, scale = 1, shape = 0, shift = 0)
#>  [1] 0.8388513 3.1086816 0.3117120 0.2506580 0.4272259 2.1590334 0.4967381
#>  [8] 3.4068405 0.3605082 0.3629492
```

While this is equivalent to using `rexp()`, the seeding is not handled
in the same way. In the following example we get two distinct samples
from the standard exponential distribution.

``` r
set.seed(1234)
rgpd_sample <- rgpd(n = 500, scale = 1, shape = 0, shift = 0)

set.seed(1234)
rexp_sample <- rexp(n = 500, rate = 1)


qqplot(
  x = rexp_sample,
  y = rgpd_sample, 
  xlim = c(0,8),
  ylim = c(0,8), 
  pch = 16,
  col = rgb(0,0,0,0.3))
abline(a = 0, b = 1, col = "darkorange")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
