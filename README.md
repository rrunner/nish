
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nish

## Overview

nish provides discrete and continuous scales and different themes to be
used with ggplot2

## Installation

Install the development version from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("rrunner/nish")
```

## Usage

``` r
library(ggplot2)
library(nish)

ggplot(economics_long, aes(date, value01, colour = variable)) +
  geom_line(size = 0.8) +
  labs(x = NULL, y = NULL, colour = NULL) +
  scale_colour_nish_blue() +
  theme_nish_blue()

ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill = density)) +
  scale_fill_gradientn_nish()

ggplot(data = mtcars) +
  geom_bar(aes(x = factor(gear), fill = factor(carb))) +
  labs(x = "gear", fill = "carb") +
  scale_fill_nish_pink() +
  theme_nish_white()
```

## Getting help and report issues

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/rrunner/nish/issues).
