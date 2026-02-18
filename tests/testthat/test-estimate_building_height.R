test_that("estimate_building_height errors on bad inputs", {

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
    crs = 3857
  )

  expect_error(estimate_building_height(1, r))
  expect_error(estimate_building_height(p, 1))

  sf::st_crs(p) <- NA
  expect_error(estimate_building_height(p, r))
})

test_that("height is correctly estimated for simple elevated building", {

  r <- terra::rast(nrows = 50, ncols = 50,
                   xmin = 0, xmax = 50,
                   ymin = 0, ymax = 50)
  terra::values(r) <- 0
  terra::crs(r) <- "EPSG:3857"

  poly <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(20,20), c(30,20), c(30,30), c(20,30), c(20,20)
      )))
    ),
    crs = 3857
  )

  mask <- terra::rasterize(terra::vect(poly), r, field = 1)
  r[!is.na(mask)] <- 20

  result <- estimate_building_height(poly, r)

  expect_true("height" %in% names(result))
  expect_equal(round(result$height, 2), 20)
})

test_that("height robust to interior dip artifact", {

  r <- terra::rast(nrows = 50, ncols = 50,
                   xmin = 0, xmax = 50,
                   ymin = 0, ymax = 50)
  terra::values(r) <- 0
  terra::crs(r) <- "EPSG:3857"

  poly <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(20,20), c(30,20), c(30,30), c(20,30), c(20,20)
      )))
    ),
    crs = 3857
  )

  mask <- terra::rasterize(terra::vect(poly), r, field = 1)
  r[!is.na(mask)] <- 20

  # introduce single low dip
  r[cellFromXY(r, cbind(25,25))] <- 0

  result <- estimate_building_height(poly, r)

  expect_true(result$height > 15)  # should not collapse to 0
})

test_that("negative heights are clamped to zero", {

  r <- terra::rast(nrows = 30, ncols = 30,
                   xmin = 0, xmax = 30,
                   ymin = 0, ymax = 30)
  terra::values(r) <- 10
  terra::crs(r) <- "EPSG:3857"

  poly <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(10,10), c(20,10), c(20,20), c(10,20), c(10,10)
      )))
    ),
    crs = 3857
  )

  # Make interior lower than outside
  mask <- terra::rasterize(terra::vect(poly), r, field = 1)
  r[!is.na(mask)] <- 5

  result <- estimate_building_height(poly, r)

  expect_equal(result$height, 0)
})

test_that("clamp can be disabled", {

  r <- terra::rast(nrows = 30, ncols = 30,
                   xmin = 0, xmax = 30,
                   ymin = 0, ymax = 30)
  terra::values(r) <- 10
  terra::crs(r) <- "EPSG:3857"

  poly <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(10,10), c(20,10), c(20,20), c(10,20), c(10,10)
      )))
    ),
    crs = 3857
  )

  mask <- terra::rasterize(terra::vect(poly), r, field = 1)
  r[!is.na(mask)] <- 5

  result <- estimate_building_height(poly, r, clamp = FALSE)

  expect_true(result$height < 0)
})

test_that("CRS mismatch is handled", {

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
    result <- estimate_building_height(p, r)
  )

  expect_true(inherits(result, "sf"))
})

