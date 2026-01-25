
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rasterpolygonizer

<!-- badges: start -->

[![R-CMD-check](https://github.com/jashonnew/rasterpolygonizer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jashonnew/rasterpolygonizer/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of rasterpolygonizer is to …

## Installation

You can install the development version of rasterpolygonizer like so:

``` r
# Install from GitHub
# install.packages("remotes")
#remotes::install_github("jashonnew/rasterpolygonizer")
```

This package provides three main functions:

1.  fill_ground_raster() – fills ground values in a raster.

2.  extract_building_edges_to_polygons() – finds edge patches in a
    raster.

3.  clean_building_polygons() – cleans and filters building polygons.

For a full example workflow using the included sample raster, see the
package vignette:

``` r
#browseVignettes("rasterpolygonizer")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rasterpolygonizer)
library(terra)
#> terra 1.8.93
library(sf)
#> Linking to GEOS 3.13.0, GDAL 3.10.1, PROJ 9.5.1; sf_use_s2() is TRUE

# Load sample raster
r_path <- system.file("extdata", "sample_raster.tif", package = "rasterpolygonizer")
r <- terra::rast(r_path)

# Run full workflow
r_filled <- fill_ground_raster(r)
edges <- extract_building_edges_to_polygons(r_filled)

mask_sf <- sf::st_as_sf(terra::as.polygons(r))
buildings_sf <- clean_building_polygons(edges$closed_edges, mask_sf)

# Visualize (optional)
library(ggplot2)
ggplot() +
  geom_sf(data = buildings_sf, fill = "lightblue", color = "darkblue") +
  ggtitle("Cleaned Building Polygons") +
  theme_minimal()
```

<img src="man/figures/README-example-1.png" width="100%" />

For detailed explanation and additional parameters, see the vignette.

The package includes a small example raster:
`inst/extdata/sample_raster.tif` This is used for testing and
demonstrating workflows.
