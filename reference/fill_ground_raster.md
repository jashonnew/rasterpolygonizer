# Fill raster values using neighborhood-based ground estimation

Applies a focal operation to fill NA values in a raster using a
clustering-based ground estimation method.

## Usage

``` r
fill_ground_raster(x, w = 15)
```

## Arguments

- x:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

- w:

  Focal window size (odd integer).

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).
