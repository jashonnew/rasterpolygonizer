test_that("remove_invalid_polys errors on bad inputs", {
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  terra::values(r) <- 1
  terra::crs(r) <- "EPSG:3857"

  p <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(1,1), c(3,1), c(3,3), c(1,3), c(1,1)
      )))
    ),
    crs = 3857
  )

  expect_error(remove_invalid_polys(1, r))
  expect_error(remove_invalid_polys(p, 1))

  sf::st_crs(p) <- NA
  expect_error(remove_invalid_polys(p, r))
})

test_that("ground-contaminated polygons are removed", {

  r <- terra::rast(nrows = 20, ncols = 20,
                   xmin = 0, xmax = 20,
                   ymin = 0, ymax = 20)
  terra::values(r) <- 10
  terra::crs(r) <- "EPSG:3857"

  poly <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(5,5), c(10,5), c(10,10), c(5,10), c(5,5)
      )))
    ),
    crs = 3857
  )

  result <- remove_invalid_polys(
    poly,
    r,
    buffer_dist = 2,
    ground_tol = 0.5
  )

  expect_equal(nrow(result), 0)
})

test_that("valid elevated polygons are kept", {

  r <- terra::rast(nrows = 20, ncols = 20,
                   xmin = 0, xmax = 20,
                   ymin = 0, ymax = 20)
  terra::values(r) <- 0
  terra::crs(r) <- "EPSG:3857"

  poly <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(5,5), c(10,5), c(10,10), c(5,10), c(5,5)
      )))
    ),
    crs = 3857
  )

  # Rasterize polygon to identify inside cells
  poly_vect <- terra::vect(poly)
  inside_mask <- terra::rasterize(poly_vect, r, field = 1)

  # Elevate values inside polygon
  r[!is.na(inside_mask)] <- 20

  result <- remove_invalid_polys(
    poly,
    r,
    buffer_dist = 2,
    ground_tol = 1
  )

  expect_equal(nrow(result), 1)
})


test_that("nested polygons are removed", {

  r <- terra::rast(nrows = 20, ncols = 20,
                   xmin = 0, xmax = 20,
                   ymin = 0, ymax = 20)
  terra::values(r) <- 0
  terra::crs(r) <- "EPSG:3857"

  outer <- sf::st_polygon(list(rbind(
    c(2,2), c(18,2), c(18,18), c(2,18), c(2,2)
  )))

  inner <- sf::st_polygon(list(rbind(
    c(5,5), c(10,5), c(10,10), c(5,10), c(5,5)
  )))

  polys <- sf::st_as_sf(
    data.frame(id = 1:2),
    geometry = sf::st_sfc(outer, inner),
    crs = 3857
  )

  # Elevate both polygons
  polys_vect <- terra::vect(polys)
  mask <- terra::rasterize(polys_vect, r, field = 1)

  r[!is.na(mask)] <- 20

  result <- remove_invalid_polys(
    polys,
    r,
    buffer_dist = 2,
    ground_tol = 1,
    remove_nested = TRUE
  )

  expect_equal(nrow(result), 1)

  # Confirm outer one is kept
  expect_equal(result$id, 1)
})


test_that("nested removal can be disabled", {

  r <- terra::rast(nrows = 50, ncols = 50,
                   xmin = 0, xmax = 50,
                   ymin = 0, ymax = 50)
  terra::values(r) <- 0
  terra::crs(r) <- "EPSG:3857"

  outer <- sf::st_polygon(list(rbind(
    c(5,5), c(45,5), c(45,45), c(5,45), c(5,5)
  )))

  inner <- sf::st_polygon(list(rbind(
    c(20,20), c(30,20), c(30,30), c(20,30), c(20,20)
  )))

  polys <- sf::st_as_sf(
    data.frame(id = 1:2),
    geometry = sf::st_sfc(outer, inner),
    crs = 3857
  )

  # Elevate ONLY polygon interiors independently
  outer_mask <- terra::rasterize(terra::vect(polys[1,]), r, field = 1)
  inner_mask <- terra::rasterize(terra::vect(polys[2,]), r, field = 1)

  r[!is.na(outer_mask)] <- 20
  r[!is.na(inner_mask)] <- 30  # distinct value to avoid equality

  result <- remove_invalid_polys(
    polys,
    r,
    buffer_dist = 1,
    ground_tol = 1,
    remove_nested = FALSE
  )

  expect_equal(nrow(result), 2)
})


test_that("CRS mismatch is handled with transformation", {

  r <- terra::rast(nrows = 10, ncols = 10,
                   xmin = 0, xmax = 10,
                   ymin = 0, ymax = 10)
  terra::values(r) <- 1
  terra::crs(r) <- "EPSG:3857"

  p <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(1,1), c(3,1), c(3,3), c(1,3), c(1,1)
      )))
    ),
    crs = 4326
  )

  expect_warning(
    result <- remove_invalid_polys(p, r)
  )

  expect_true(inherits(result, "sf"))
})

test_that("polygon with no raster overlap is removed", {
  r <- terra::rast(nrows = 10, ncols = 10,
                   xmin = 0, xmax = 10,
                   ymin = 0, ymax = 10)
  terra::values(r) <- 1
  terra::crs(r) <- "EPSG:3857"

  poly <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(20,20), c(25,20), c(25,25), c(20,25), c(20,20)
      )))
    ),
    crs = 3857
  )

  result <- remove_invalid_polys(poly, r)

  expect_equal(nrow(result), 0)
})


