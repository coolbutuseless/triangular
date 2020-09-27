
<!-- README.md is generated from README.Rmd. Please edit that file -->

# triangular

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg) [![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

`triangular` decomposes complex polygons into sets of triangles and
works with:

  - polygons with holes
  - self-intersecting polygons

This package relies heavily on
[RTriangle](https://cran.r-project.org/package=RTriangle) - which is
licensed [CC
BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0).

## Installation

You can install from
[GitHub](https://github.com/coolbutuseless/triangular) with:

``` r
# install.package('remotes')
remotes::install_github('coolbutuseless/triangular')
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(triangular)
```

## Polygon with a Hole in it

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# polygons_df - data.frame of polygon vertices with group/subgroups
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
polygons_df <- df <- data.frame(
  x        = c(4, 8, 8, 4,   6, 7, 7, 6),
  y        = c(4, 4, 8, 8,   6, 6, 7, 7),
  group    = c(1, 1, 1, 1,   1, 1, 1, 1),
  subgroup = c(1, 1, 1, 1,   2, 2, 2, 2)
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How 'ggplot2' handles this case
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(polygons_df) +
  geom_polygon(aes(x, y, group=group, subgroup=subgroup)) +
  geom_path(aes(x, y, group = interaction(group, subgroup)), colour = 'red') +
  theme_bw() + 
  coord_equal() + 
  labs(title = "ggplot2 rendering of original polygon(s)")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="60%" />

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Turn the polygon data.frame into individual triangles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
res <- triangular::decompose(polygons_df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove the triangles which are 'interior' according to the even-odd rule
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
triangles_df <- res$triangles_df %>%
  filter(interior)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot the triangles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(triangles_df) +
  geom_polygon(aes(x, y, group = idx), alpha = 0.3, colour = 'blue') +
  theme_bw() + 
  coord_equal() + 
  labs(title = "Decomposition into simple tris with {triangular}")
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="60%" />

## Polygon with Two Holes

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
poly <- df <- data.frame(
  x        = c(4, 8, 8, 4,   6, 7, 7, 6,  4.5,   5, 5, 4.5),
  y        = c(4, 4, 8, 8,   6, 6, 7, 7,  4.5, 4.5, 5, 5),
  group    = c(1, 1, 1, 1,   1, 1, 1, 1,    1,   1, 1, 1),
  subgroup = c(1, 1, 1, 1,   2, 2, 2, 2,    3,   3, 3, 3)
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "Native" ggplot2 rendering
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(df) +
  geom_polygon(aes(x, y, subgroup = subgroup), colour = 'red') + 
  theme_bw() + 
  coord_equal() + 
  labs(title = "ggplot2 rendering of original polygon(s)")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="60%" />

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Decompose into triangles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
res <- triangular::decompose(poly)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove the triangles which are 'interior' according to the even-odd rule
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
triangles_df <- res$triangles_df %>%
  filter(interior)

ggplot(triangles_df) +
  geom_polygon(aes(x, y, group = idx), alpha = 0.3, colour = 'blue') +
  theme_bw() + 
  coord_equal() + 
  labs(title = "Decomposition into simple tris with {triangular}")
```

<img src="man/figures/README-unnamed-chunk-3-2.png" width="60%" />

## Two Polygons with One Hole Each

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Two polygons with one hole each
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
poly <- df <- data.frame(
  x        = c(1, 4, 4, 1,  2, 3, 3, 2,      5, 8, 8, 5,  6, 7, 7, 6),
  y        = c(1, 1, 4, 4,  2, 2, 3, 3,      5, 5, 8, 8,  6, 6, 7, 7),
  group    = c(1, 1, 1, 1,  1, 1, 1, 1,      1, 1, 1, 1,  1, 1, 1, 1),
  subgroup = c(1, 1, 1, 1,  2, 2, 2, 2,      3, 3, 3, 3,  4, 4, 4, 4)
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "Native" ggplot2 rendering
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(df) +
  geom_polygon(aes(x, y, subgroup = subgroup), colour = 'red') + 
  theme_bw() + 
  coord_equal() + 
  labs(title = "ggplot2 rendering of original polygon(s)")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="60%" />

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Decompose into triangles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
res <- triangular::decompose(poly)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove the triangles which are 'interior' according to the even-odd rule
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
triangles_df <- res$triangles_df %>%
  filter(interior)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Manual rendering of triangles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(triangles_df) +
  geom_polygon(aes(x, y, group = idx), alpha = 0.3, colour = 'blue') +
  theme_bw() +
  coord_equal() + 
  labs(title = "Decomposition into simple tris with {triangular}")
```

<img src="man/figures/README-unnamed-chunk-4-2.png" width="60%" />

## Polygon from Random Points

``` r
set.seed(1)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 10 random points
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
polygons_df <- data.frame(
  x        = runif(10),
  y        = runif(10),
  group    = 1,
  subgroup = 1
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "Native" ggplot2 rendering
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(polygons_df) +
  geom_polygon(aes(x, y)) +
  geom_path(aes(x, y, group = interaction(group, subgroup)), colour = 'red') +
  theme_bw() + 
  coord_equal() + 
  labs(title = "ggplot2 rendering of original polygon(s)")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="60%" />

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Decompose into triangles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
res <- triangular::decompose(polygons_df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove the triangles which are 'interior' according to the even-odd rule
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
triangles_df <- res$triangles_df %>%
  filter(interior)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Manual rendering of triangles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(triangles_df) +
  geom_polygon(aes(x, y, group = idx), alpha = 0.3, colour = 'blue') +
  theme_bw() + 
  coord_equal() + 
  labs(title = "Decomposition into simple tris with {triangular}")
```

<img src="man/figures/README-unnamed-chunk-5-2.png" width="60%" />

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Polyclip processing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A <- split(polygons_df, interaction(polygons_df$group, polygons_df$subgroup))
simplified_list <- polyclip::polysimplify(A)

simplified_df <- lapply(
  seq_along(simplified_list),
  function(idx) {
    res <- as.data.frame(simplified_list[[idx]])
    res$idx <- idx
    res
  }
)
simplified_df <- do.call(rbind, simplified_df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Manual rendering of triangles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(simplified_df) +
  geom_polygon(aes(x, y, group = idx), alpha = 0.3, colour = 'blue') +
  theme_bw() + 
  coord_equal() + 
  labs(title = "Decomposition into simple tris with 'polyclip::polysimplify()'")
```

<img src="man/figures/README-unnamed-chunk-5-3.png" width="60%" />