test_that("ground_fill_neighborhood handles NA center pixel", {

  neighbors <- c(
    10, 11, 12,
    11, NA, 50,
    10, 11, 12
  )

  val <- ground_fill_neighborhood(neighbors)

  expect_true(is.numeric(val))
  expect_false(is.na(val))
  expect_lt(val, 20)  # ground should be low, not the 50
})

test_that("fill_ground_raster returns a raster of same dimensions", {

  r_path <- system.file(
    "extdata",
    "sample_raster.tif",
    package = "rasterpolygonizer"
  )

  r <- terra::rast(r_path)
  r_filled <- fill_ground_raster(r, w = 15)

  expect_s4_class(r_filled, "SpatRaster")
  expect_equal(dim(r), dim(r_filled))
})
