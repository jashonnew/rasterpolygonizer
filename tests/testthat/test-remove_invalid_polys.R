# tests/testthat/test-remove_invalid_polys.R

library(testthat)
library(sf)
library(terra)

# ---- Helper: build a minimal raster + polygon setup ----

make_raster <- function(vals, nrow = 10, ncol = 10,
                        xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                        crs = "EPSG:32610") {
  r <- terra::rast(
    nrows = nrow, ncols = ncol,
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    crs = crs
  )
  terra::values(r) <- vals
  r
}

make_poly <- function(xmin, xmax, ymin, ymax, crs = "EPSG:32610") {
  sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(xmin, ymin), c(xmax, ymin),
        c(xmax, ymax), c(xmin, ymax),
        c(xmin, ymin)
      ))),
      crs = crs
    )
  )
}

# ---- Input validation ----

test_that("errors on non-sf polys", {
  r <- make_raster(rep(10, 100))
  expect_error(remove_invalid_polys(data.frame(), r), "polys must be an sf object")
})

test_that("errors on non-SpatRaster raster", {
  p <- make_poly(2, 4, 2, 4)
  expect_error(remove_invalid_polys(p, matrix(1:100, 10)), "terra::SpatRaster")
})

test_that("errors on missing CRS in polys", {
  r <- make_raster(rep(10, 100))
  p <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(2,2),c(4,2),c(4,4),c(2,4),c(2,2))))
  ))  # no CRS
  expect_error(remove_invalid_polys(p, r), "missing CRS")
})

test_that("errors on missing CRS in raster", {
  r <- make_raster(rep(10, 100))
  terra::crs(r) <- ""
  p <- make_poly(2, 4, 2, 4)
  expect_error(remove_invalid_polys(p, r), "missing CRS")
})

# ---- CRS mismatch ----

test_that("warns and proceeds on CRS mismatch", {
  r <- make_raster(rep(10, 100), crs = "EPSG:32610")
  # Build a poly in geographic coords that overlaps when reprojected
  p <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(-121.9, 36.0), c(-121.8, 36.0),
      c(-121.8, 36.1), c(-121.9, 36.1),
      c(-121.9, 36.0)
    ))),
    crs = "EPSG:4326"
  ))
  # Just check warning is raised; result may be empty due to extent
  expect_warning(
    remove_invalid_polys(p, r, buffer_dist = 1, ground_tol = 0.1),
    "CRS mismatch"
  )
})

# ---- Ground contamination removal (Step 1) ----

test_that("keeps polygon when inside min is much higher than outside min", {
  # Raster: high values (10) everywhere except the outer border (~ground)
  vals <- rep(10, 100)
  # Lower outside values along border rows/cols
  border_idx <- c(1:10, 91:100, seq(1, 91, 10), seq(10, 100, 10))
  vals[border_idx] <- 1
  r <- make_raster(vals)

  # Polygon covers interior cells (high values)
  p <- make_poly(3, 7, 3, 7)

  result <- remove_invalid_polys(p, r, buffer_dist = 1, ground_tol = 3)
  expect_equal(nrow(result), 1)
})

test_that("removes polygon when inside min is similar to outside min (ground-contaminated)", {
  # Flat raster — min inside ~ min outside, difference < ground_tol
  r <- make_raster(rep(5, 100))
  p <- make_poly(2, 5, 2, 5)

  result <- remove_invalid_polys(p, r, buffer_dist = 1, ground_tol = 3)
  expect_equal(nrow(result), 0)
})

test_that("removes polygon when inside values are all NA", {
  r <- make_raster(rep(NA_real_, 100))
  p <- make_poly(2, 5, 2, 5)

  result <- remove_invalid_polys(p, r, buffer_dist = 1, ground_tol = 0.25)
  expect_equal(nrow(result), 0)
})

test_that("keeps polygon when outside ring has no raster values", {
  # Polygon fills nearly the entire raster extent — ring falls outside
  r <- make_raster(rep(10, 100))
  p <- make_poly(0.1, 9.9, 0.1, 9.9)

  result <- remove_invalid_polys(p, r, buffer_dist = 0.05, ground_tol = 0.25)
  expect_equal(nrow(result), 1)
})

# ---- Nested polygon removal (Step 2) ----

test_that("removes polygon nested inside another polygon", {
  r <- make_raster(c(rep(1, 50), rep(10, 50)))  # high enough contrast

  # Make both polys clearly above ground so both survive step 1
  vals <- rep(1, 100)
  # Set inner region high, outer region lower but still > tol from inside
  r2 <- make_raster(vals)
  terra::values(r2) <- 1
  # Override: use a flat-high raster and large tolerance so step 1 keeps all
  r_flat_high <- make_raster(rep(10, 100))

  outer_poly <- make_poly(1, 9, 1, 9)
  inner_poly <- make_poly(3, 6, 3, 6)
  polys <- rbind(outer_poly, inner_poly)

  # With a flat raster both are "ground contaminated" — craft values so
  # both survive step 1 by having inside >> outside
  # Use a raster with high interior, low exterior
  vals2 <- rep(1, 100)  # default low
  # cells roughly in 1-9 x 1-9 range: rows 2-9, cols 2-9 (approx)
  # easier: just set all to 10, ground_tol = 0 so everything passes step 1
  r_pass <- make_raster(rep(10, 100))

  result_nested <- remove_invalid_polys(
    polys, r_pass,
    buffer_dist = 0.5, ground_tol = 100,  # huge tol => step 1 keeps all
    remove_nested = TRUE
  )
  result_keep <- remove_invalid_polys(
    polys, r_pass,
    buffer_dist = 0.5, ground_tol = 100,
    remove_nested = FALSE
  )

  expect_lt(nrow(result_nested), nrow(result_keep))
  expect_equal(nrow(result_nested), 1)
  expect_equal(nrow(result_keep), 2)
})

test_that("remove_nested = FALSE leaves nested polygons in place", {
  r <- make_raster(rep(10, 100))
  outer_poly <- make_poly(1, 9, 1, 9)
  inner_poly <- make_poly(3, 6, 3, 6)
  polys <- rbind(outer_poly, inner_poly)

  result <- remove_invalid_polys(
    polys, r,
    buffer_dist = 0.5, ground_tol = 100,
    remove_nested = FALSE
  )
  expect_equal(nrow(result), 2)
})

# ---- Return type ----

test_that("returns an sf object", {
  r <- make_raster(rep(10, 100))
  p <- make_poly(2, 5, 2, 5)
  result <- remove_invalid_polys(p, r, buffer_dist = 1, ground_tol = 100)
  expect_s3_class(result, "sf")
})

test_that("returns empty sf (not error) when all polygons are filtered", {
  r <- make_raster(rep(5, 100))
  p <- make_poly(2, 5, 2, 5)
  result <- remove_invalid_polys(p, r, buffer_dist = 1, ground_tol = 100)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 0)
})
