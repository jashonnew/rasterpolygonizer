#' Estimate building height from raster
#'
#' Computes building height as the difference between a lower percentile
#' of raster values inside the polygon and the same percentile in a
#' surrounding buffer ring.
#'
#' @param polys sf polygon object.
#' @param raster terra::SpatRaster with elevation values.
#' @param buffer_dist Numeric buffer distance (map units). Default 5.
#' @param prob Quantile probability used for robust lower surface estimate.
#'   Default 0.02.
#' @param clamp Logical; if TRUE, negative heights are set to 0.
#'
#' @return sf object with added columns:
#'   - inside_q
#'   - outside_q
#'   - height
#' @export
estimate_building_height <- function(
    polys,
    raster,
    buffer_dist = 5,
    prob = 0.02,
    clamp = TRUE
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

  if (!terra::same.crs(raster, polys)) {
    warning("CRS mismatch detected. Transforming polygons to raster CRS.")
    polys <- sf::st_transform(polys, terra::crs(raster))
  }

  polys_vect <- terra::vect(polys)

  inside_q <- numeric(nrow(polys))
  outside_q <- numeric(nrow(polys))
  height <- numeric(nrow(polys))

  for (i in seq_len(nrow(polys))) {

    poly_sf <- polys[i, ]
    poly_vect <- polys_vect[i]

    # --- Inside values ---
    inside_vals <- terra::extract(raster, poly_vect)[, 2]
    inside_vals <- inside_vals[!is.na(inside_vals)]

    if (length(inside_vals) == 0) {
      inside_q[i] <- NA
      outside_q[i] <- NA
      height[i] <- NA
      next
    }

    inside_q[i] <- as.numeric(stats::quantile(
      inside_vals,
      probs = prob,
      names = FALSE
    ))

    # --- Outside ring ---
    buffered <- sf::st_buffer(poly_sf, buffer_dist)
    ring <- sf::st_difference(buffered, poly_sf)

    if (nrow(ring) == 0) {
      outside_q[i] <- NA
      height[i] <- NA
      next
    }

    ring_vect <- terra::vect(ring)
    outside_vals <- terra::extract(raster, ring_vect)[, 2]
    outside_vals <- outside_vals[!is.na(outside_vals)]

    if (length(outside_vals) == 0) {
      outside_q[i] <- NA
      height[i] <- NA
      next
    }

    outside_q[i] <- as.numeric(stats::quantile(
      outside_vals,
      probs = prob,
      names = FALSE
    ))

    height[i] <- inside_q[i] - outside_q[i]

    if (clamp && !is.na(height[i])) {
      height[i] <- max(height[i], 0)
    }
  }

  polys$inside_q <- inside_q
  polys$outside_q <- outside_q
  polys$height <- height

  polys
}

