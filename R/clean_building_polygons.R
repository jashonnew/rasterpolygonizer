#' Clean building polygons from closed edges raster
#'
#' Convert a raster of closed edges into clean, simplified, filtered polygons.
#'
#' @param closed_edges Raster layer (terra SpatRaster) with TRUE/FALSE edges
#' @param mask_shape sf object to crop/mask raster (optional CRS)
#' @param shrink_dist Distance to shrink polygons (negative buffer), default -0.5
#' @param simplify_tol Distance tolerance for simplification, default 0.5
#' @param min_area Minimum polygon area to keep
#' @param max_area Maximum polygon area to keep
#' @return sf object with cleaned building polygons
#' @export
clean_building_polygons <- function(
    closed_edges,
    mask_shape = NULL,
    shrink_dist = -0.5,
    simplify_tol = 0.5,
    min_area = 50,
    max_area = 8000
) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package sf is required")
  if (!inherits(closed_edges, "SpatRaster")) stop("closed_edges must be a terra SpatRaster")
  if (!is.null(mask_shape) && !inherits(mask_shape, "sf")) {
    stop("mask_shape must be an sf object if provided")
  }

  # Crop and mask raster
  if (!is.null(mask_shape)) {
    r_crop   <- terra::crop(closed_edges, mask_shape)
    r_masked <- terra::mask(r_crop, mask_shape)
  } else {
    r_masked <- closed_edges
  }

  # Convert raster to polygons
  building_polys <- terra::as.polygons(r_masked, dissolve = TRUE)
  building_sf <- sf::st_as_sf(building_polys)

  # Shrink polygons
  building_sf <- sf::st_buffer(building_sf, dist = shrink_dist)
  building_sf <- sf::st_make_valid(building_sf)

  # Simplify polygons
  building_sf <- sf::st_simplify(building_sf, dTolerance = simplify_tol, preserveTopology = TRUE)

  # --- SPLIT INTO INDIVIDUAL BUILDINGS ---
  building_sf <- sf::st_cast(building_sf, "POLYGON", warn = FALSE)

  # Assign unique building IDs
  building_sf$building_id <- seq_len(nrow(building_sf))

  # Drop empty or invalid geometries
  building_sf <- building_sf[
    !sf::st_is_empty(building_sf) &
      sf::st_is_valid(building_sf),
  ]

  # --- NOW compute geometry metrics ---
  building_sf$area  <- as.numeric(sf::st_area(building_sf))
  building_sf$perim <- as.numeric(sf::st_length(building_sf))
  building_sf$perim_area_ratio <- building_sf$perim / building_sf$area


  # Compute number of vertices
  building_sf$vertices <- sapply(sf::st_geometry(building_sf), function(x) {
    sum(sapply(x, function(polygon) nrow(polygon)))
  })

  # Filter polygons by area
  building_sf <- building_sf[building_sf$area > min_area & building_sf$area < max_area, , drop = FALSE]

  # Transform CRS only if both have valid CRS
  if (!is.null(mask_shape) &&
      !is.na(sf::st_crs(mask_shape)) &&
      !is.na(sf::st_crs(building_sf))) {
    building_sf <- sf::st_transform(building_sf, sf::st_crs(mask_shape))
  }

  building_sf
}
