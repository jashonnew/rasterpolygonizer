#' Extract building edges and convert to polygons
#'
#' Processes a SpatRaster to detect building edges using smoothing,
#' Sobel gradients, thresholding, and morphological operations,
#' returning both raster layers and polygon objects.
#'
#' @importFrom stats quantile
#' @param input_raster A `terra::SpatRaster`.
#' @param smooth_w Smoothing window size (odd integer).
#' @param sobel_w Sobel kernel window (must be 3).
#' @param thr_prob Quantile for thresholding gradient magnitude.
#' @param morph_w Morphological kernel size (odd integer).
#' @param min_area Minimum polygon area to keep (requires `sf`).
#' @return A list with raster layers and polygons (terra + sf if available)
#' @export
extract_building_edges_to_polygons <- function(
    input_raster,
    smooth_w = 3,
    sobel_w = 3,
    thr_prob = 0.70,
    morph_w = 3,
    min_area = 10
) {
  if (!inherits(input_raster, "SpatRaster")) stop("input_raster must be a terra SpatRaster")
  if (sobel_w != 3) stop("This implementation uses the standard 3x3 Sobel kernels (sobel_w = 3).")
  if ((smooth_w %% 2) == 0 || (morph_w %% 2) == 0) stop("smooth_w and morph_w must be odd integers.")

  r <- terra::mask(input_raster, !is.na(input_raster))

  # smoothing
  smooth_kernel <- matrix(1, nrow = smooth_w, ncol = smooth_w)
  smooth <- terra::focal(r, w = smooth_kernel, fun = function(v) {
    if (all(is.na(v))) return(NA_real_)
    mean(v, na.rm = TRUE)
  })

  # Sobel kernels
  kx <- c(-1, 0, 1,
          -2, 0, 2,
          -1, 0, 1)
  ky <- c(-1, -2, -1,
          0, 0, 0,
          1, 2, 1)

  sobel_window <- matrix(1, nrow = sobel_w, ncol = sobel_w)
  gx <- terra::focal(smooth, w = sobel_window, fun = function(v) conv_with_kernel(v, kx))
  gy <- terra::focal(smooth, w = sobel_window, fun = function(v) conv_with_kernel(v, ky))

  grad <- sqrt(gx^2 + gy^2)
  thr_value <- as.numeric(stats::quantile(terra::values(grad), probs = thr_prob, na.rm = TRUE))
  edges_bin <- grad > thr_value
  edges_num <- as.numeric(edges_bin)

  # morphological closing
  morph_kernel <- matrix(1, nrow = morph_w, ncol = morph_w)
  dilated <- terra::focal(edges_num, w = morph_kernel, fun = function(v) {
    if (all(is.na(v))) return(NA_real_)
    max(v, na.rm = TRUE)
  })
  closed <- terra::focal(dilated, w = morph_kernel, fun = function(v) {
    if (all(is.na(v))) return(NA_real_)
    min(v, na.rm = TRUE)
  })
  closed_edges <- closed == 1

  # convert to patches and polygons
  patch_r <- terra::patches(closed_edges, directions = 8)
  patch_polys <- terra::as.polygons(patch_r, dissolve = TRUE)

  polys_sf <- NULL
  if (requireNamespace("sf", quietly = TRUE)) {
    polys_sf <- sf::st_as_sf(patch_polys)
    polys_sf$area_m2 <- as.numeric(sf::st_area(polys_sf))
    polys_sf <- polys_sf[polys_sf$area_m2 > min_area, , drop = FALSE]
  } else {
    message("sf not available: returning terra SpatVector. Install sf to filter polygons by area.")
  }

  list(
    smooth = smooth,
    gx = gx,
    gy = gy,
    grad = grad,
    thresh_value = thr_value,
    edges_binary = edges_bin,
    closed_edges = closed_edges,
    patch_r = patch_r,
    patch_polys_terra = patch_polys,
    patch_polys_sf = polys_sf
  )
}
