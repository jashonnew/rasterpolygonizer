#' Remove invalid building polygons
#'
#' Cleans building polygons in two ordered steps:
#'
#' 1. Removes polygons that likely include ground values by comparing
#'    minimum raster values inside the polygon to those in a surrounding
#'    buffer ring.
#' 2. Removes polygons completely contained within other polygons.
#'
#' The order is important: ground-contaminated polygons are removed first
#' to avoid incorrectly discarding valid smaller polygons in favor of
#' invalid larger ones.
#'
#' @param polys An `sf` polygon object.
#' @param raster A `terra::SpatRaster` used to evaluate elevation values.
#' @param buffer_dist Numeric buffer distance (map units) used to create
#'   the exterior comparison ring. Default is 1.
#' @param ground_tol Numeric tolerance for difference between inside and
#'   outside minimum raster values. If the absolute difference is less than
#'   this value, the polygon is considered ground-contaminated. Default 0.25.
#' @param remove_nested Logical; if `TRUE`, polygons fully contained within
#'   other polygons are removed after ground filtering. Default `TRUE`.
#'
#' @return A cleaned `sf` polygon object.
#' @export
remove_invalid_polys <- function(
    polys,
    raster,
    buffer_dist = 5,
    ground_tol = 3,
    remove_nested = TRUE
) {

  # ---- Dependency checks ----
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' required for this function.")
  }

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' required for this function.")
  }

  # ---- Input checks ----
  if (!inherits(polys, "sf")) {
    stop("polys must be an sf object.")
  }

  if (!inherits(raster, "SpatRaster")) {
    stop("raster must be a terra::SpatRaster.")
  }

  if (is.na(sf::st_crs(polys))) {
    stop("polys has missing CRS.")
  }

  if (is.na(terra::crs(raster))) {
    stop("raster has missing CRS.")
  }

  # ---- CRS alignment ----
  if (!terra::same.crs(raster, polys)) {
    warning("CRS mismatch detected. Transforming polygons to raster CRS.")
    polys <- sf::st_transform(polys, terra::crs(raster))
  }

  # Convert to terra vector once
  polys_vect <- terra::vect(polys)

  keep_idx <- logical(nrow(polys))

  # ---- Step 1: Remove ground-contaminated polygons ----
  for (i in seq_len(nrow(polys))) {

    poly_sf <- polys[i, ]
    poly_vect <- polys_vect[i]

    # Extract inside values
    inside_vals <- terra::extract(raster, poly_vect)[, 2]
    inside_vals <- inside_vals[!is.na(inside_vals)]

    if (length(inside_vals) == 0) {
      keep_idx[i] <- FALSE
      next
    }

    min_inside <- min(inside_vals)

    # Create buffer ring
    buffered <- sf::st_buffer(poly_sf, buffer_dist)
    ring <- sf::st_difference(buffered, poly_sf)

    if (nrow(ring) == 0) {
      keep_idx[i] <- TRUE
      next
    }

    ring_vect <- terra::vect(ring)
    outside_vals <- terra::extract(raster, ring_vect)[, 2]
    outside_vals <- outside_vals[!is.na(outside_vals)]

    if (length(outside_vals) == 0) {
      keep_idx[i] <- TRUE
      next
    }

    min_outside <- min(outside_vals)

    keep_idx[i] <- abs(min_inside - min_outside) > ground_tol
  }

  cleaned <- polys[keep_idx, , drop = FALSE]

  # ---- Step 2: Remove nested polygons ----
  if (remove_nested && nrow(cleaned) > 1) {

    within_matrix <- sf::st_within(cleaned, cleaned, sparse = FALSE)

    diag(within_matrix) <- FALSE

    nested <- apply(within_matrix, 1, any)

    cleaned <- cleaned[!nested, , drop = FALSE]
  }

  cleaned
}
