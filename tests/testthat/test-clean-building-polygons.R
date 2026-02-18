test_that("clean_building_polygons works on sample raster", {
  skip_if_not_installed("sf")

  library(terra)
  library(sf)

  # Load sample raster
  r_path <- system.file("extdata", "sample_raster.tif", package = "rasterpolygonizer")
  r <- terra::rast(r_path)

  # Dummy "closed edges" raster: use fill_ground_raster > median
  r_filled <- fill_ground_raster(r)
  closed_edges <- r_filled > quantile(terra::values(r_filled), 0.5, na.rm = TRUE)

  # Create a simple mask polygon from raster extent
  mask_poly <- terra::as.polygons(r)
  mask_sf <- sf::st_as_sf(mask_poly)

  # Run the cleaning function
  buildings_sf <- clean_building_polygons(
    closed_edges = closed_edges,
    mask_shape = mask_sf,
    shrink_dist = -0.5,
    simplify_tol = 0.5,
    min_area = 1,      # small for test raster
    max_area = 1e6
  )

  # --- Tests ---
  expect_s3_class(buildings_sf, "sf")
  expect_true(all(sf::st_is_valid(buildings_sf)))
  expect_true(all(buildings_sf$area > 0))
  expect_true("perim_area_ratio" %in% names(buildings_sf))
  expect_true(all(sf::st_geometry_type(buildings_sf) %in% c("POLYGON", "MULTIPOLYGON")))
})
