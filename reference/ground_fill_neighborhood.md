# Estimate ground value from a focal neighborhood

Given a vector of neighboring raster values (including the center
pixel), returns a representative ground value using clustering-based
heuristics.

## Usage

``` r
ground_fill_neighborhood(neighbors)
```

## Arguments

- neighbors:

  Numeric vector of focal window values.

## Value

A single numeric value or NA.
