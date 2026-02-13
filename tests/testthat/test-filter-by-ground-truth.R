test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

library(sf)

make_square <- function(xmin, ymin, size = 10, crs = 32606) {
  coords <- matrix(c(
    xmin, ymin,
    xmin + size, ymin,
    xmin + size, ymin + size,
    xmin, ymin + size,
    xmin, ymin
  ), ncol = 2, byrow = TRUE)

  sf::st_sf(
    geometry = sf::st_sfc(sf::st_polygon(list(coords)), crs = crs)
  )
}

test_that("filter_by_ground_truth keeps contained polygons", {

  truth <- make_square(0, 0, size = 10)
  candidate_inside <- make_square(1, 1, size = 5)
  candidate_outside <- make_square(20, 20, size = 5)

  candidates <- rbind(candidate_inside, candidate_outside)

  result <- filter_by_ground_truth(candidates, truth, threshold = 0.75)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1)
})

test_that("threshold affects filtering", {

  truth <- make_square(0, 0, size = 10)
  candidate_partial <- make_square(5, 5, size = 10)  # 25% overlap

  result_high <- filter_by_ground_truth(candidate_partial, truth, threshold = 0.5)
  result_low  <- filter_by_ground_truth(candidate_partial, truth, threshold = 0.1)

  expect_equal(nrow(result_high), 0)
  expect_equal(nrow(result_low), 1)
})

test_that("CRS mismatch is handled", {

  truth <- make_square(0, 0, size = 10, crs = 32606)
  candidate <- make_square(0, 0, size = 5, crs = 3857)

  expect_warning(
    result <- filter_by_ground_truth(candidate, truth),
    "CRS mismatch"
  )

  expect_s3_class(result, "sf")
})

test_that("missing CRS in candidates warns", {

  truth <- make_square(0, 0, size = 10)

  candidate <- make_square(0, 0, size = 5)
  sf::st_crs(candidate) <- NA

  expect_error(
    filter_by_ground_truth(candidate, truth),
    "candidates has missing CRS"
  )
})

test_that("missing CRS in truth errors", {

  truth <- make_square(0, 0, size = 10)
  sf::st_crs(truth) <- NA

  candidate <- make_square(0, 0, size = 5)

  expect_error(
    filter_by_ground_truth(candidate, truth),
    "truth has missing CRS"
  )
})

test_that("non-sf input errors", {

  truth <- make_square(0, 0, size = 10)

  expect_error(
    filter_by_ground_truth(data.frame(x = 1), truth),
    "must be sf objects"
  )
})

