testthat::test_that("roof_slope_RANSAC returns expected structure", {

  skip_if_not_installed("terra")

  set.seed(123)

  # --- Create synthetic planar raster ---
  r <- terra::rast(nrows = 20, ncols = 20,
                   xmin = 0, xmax = 10,
                   ymin = 0, ymax = 10)

  xy <- terra::xyFromCell(r, 1:terra::ncell(r))

  # Plane: z = 2 + 0.1x + 0.2y
  z <- 2 + 0.1 * xy[,1] + 0.2 * xy[,2]
  terra::values(r) <- z

  # --- Create single square building ---
  b <- terra::vect(matrix(
    c(2,2,
      8,2,
      8,8,
      2,8,
      2,2),
    ncol = 2,
    byrow = TRUE
  ), type = "polygons")

  b$building_id <- "A"

  result <- roof_slope_RANSAC(
    raster = r,
    buildings = b,
    n_iter = 100,
    thresh = 0.001,
    min_inliers = 20,
    quiet = TRUE
  )

  # --- Structure checks ---
  testthat::expect_type(result, "list")
  testthat::expect_named(result, c("results_list", "summary_table"))

  testthat::expect_true("A" %in% names(result$results_list))
  testthat::expect_s3_class(result$summary_table, "data.frame")

})

testthat::test_that("roof_slope_RANSAC estimates correct slope for known plane", {

  skip_if_not_installed("terra")

  set.seed(42)

  r <- terra::rast(nrows = 20, ncols = 20,
                   xmin = 0, xmax = 10,
                   ymin = 0, ymax = 10)

  xy <- terra::xyFromCell(r, 1:terra::ncell(r))
  z <- 2 + 0.1 * xy[,1] + 0.2 * xy[,2]
  terra::values(r) <- z

  b <- terra::vect(matrix(
    c(1,1,
      9,1,
      9,9,
      1,9,
      1,1),
    ncol = 2,
    byrow = TRUE
  ), type = "polygons")

  b$building_id <- "B"

  result <- roof_slope_RANSAC(
    r, b,
    n_iter = 200,
    thresh = 0.0001,
    min_inliers = 50,
    quiet = TRUE
  )

  expected_slope <- atan(sqrt(0.1^2 + 0.2^2)) * 180/pi

  estimated_slope <- result$summary_table$slope_deg

  testthat::expect_equal(
    estimated_slope,
    expected_slope,
    tolerance = 1e-2
  )
})

testthat::test_that("returns empty results if not enough inliers", {

  skip_if_not_installed("terra")

  r <- terra::rast(nrows = 5, ncols = 5,
                   xmin = 0, xmax = 5,
                   ymin = 0, ymax = 5)

  terra::values(r) <- runif(terra::ncell(r))

  b <- terra::vect(matrix(
    c(1,1,
      4,1,
      4,4,
      1,4,
      1,1),
    ncol = 2,
    byrow = TRUE
  ), type = "polygons")

  b$building_id <- "C"

  result <- roof_slope_RANSAC(
    r, b,
    n_iter = 50,
    min_inliers = 1000, # impossible
    quiet = TRUE
  )

  testthat::expect_equal(length(result$results_list), 0)
})

testthat::test_that("stability metric small for perfect plane", {

  skip_if_not_installed("terra")

  set.seed(1)

  r <- terra::rast(nrows = 20, ncols = 20,
                   xmin = 0, xmax = 10,
                   ymin = 0, ymax = 10)

  terra::values(r) <- 5  # perfectly flat

  b <- terra::vect(matrix(
    c(2,2,
      8,2,
      8,8,
      2,8,
      2,2),
    ncol = 2,
    byrow = TRUE
  ), type = "polygons")

  b$building_id <- "D"

  result <- roof_slope_RANSAC(
    r, b,
    n_iter = 100,
    thresh = 0.0001,
    min_inliers = 20,
    quiet = TRUE
  )

  spread <- result$summary_table$slope_spread

  testthat::expect_true(is.na(spread) || spread < 1e-3)
})

testthat::test_that("errors on bad inputs", {

  skip_if_not_installed("terra")

  r <- 1:10
  b <- 1:5

  testthat::expect_error(
    roof_slope_RANSAC(r, b),
    "SpatRaster"
  )
})

