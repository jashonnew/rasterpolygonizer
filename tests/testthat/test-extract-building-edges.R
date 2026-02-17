test_that("extract_building_edges_to_polygons runs on sample raster", {
  r_path <- system.file("extdata", "sample_raster.tif", package = "rasterpolygonizer")
  r <- terra::rast(r_path)

  out <- extract_building_edges_to_polygons(r, smooth_w = 3, morph_w = 3)

  expect_s4_class(out$smooth, "SpatRaster")
  expect_s4_class(out$gx, "SpatRaster")
  expect_s4_class(out$gy, "SpatRaster")
  expect_s4_class(out$grad, "SpatRaster")
  expect_true(!is.null(out$thresh_value))
  expect_s4_class(out$patch_polys_terra, "SpatVector")
})
