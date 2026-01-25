#' Clean building polygons from closed edges raster
#'
#' Convert a raster of closed edges into clean, simplified, filtered polygons.
#'
#' @param closed_edges Raster layer (terra SpatRaster) with TRUE/FALSE edges
#' @param mask_shape sf object to crop/mask raster (optional CRS)
#' @param shrink_dist Distance to shrink polygons (negative buffer), default -0.5
#' @param simplify_keep Fraction to retain when simplifying, default 0.95
#' @param min_area Minimum polygon area to keep
#' @param max_area Maximum polygon area to keep
#' @importFrom rmapshaper ms_simplify
#' @return sf object with cleaned building polygons
#' @export
clean_building_polygons <- function(
    closed_edges,
    mask_shape,
    shrink_dist = -0.5,
    simplify_keep = 0.95,
    min_area = 50,
    max_area = 8000
) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package sf is required")
  if (!requireNamespace("rmapshaper", quietly = TRUE)) stop("Package rmapshaper is required")
  if (!inherits(closed_edges, "SpatRaster")) stop("closed_edges must be a terra SpatRaster")
  if (!inherits(mask_shape, "sf")) stop("mask_shape must be an sf object")

  # Crop and mask raster
  r_crop <- terra::crop(closed_edges, mask_shape)
  r_masked <- terra::mask(r_crop, mask_shape)

  # Convert raster to polygons
  building_polys <- terra::as.polygons(r_masked, dissolve = TRUE)
  building_sf <- sf::st_as_sf(building_polys)

  # Shrink polygons
  building_sf <- sf::st_buffer(building_sf, dist = shrink_dist)
  building_sf <- sf::st_make_valid(building_sf)

  # Simplify polygons
  building_sf <- rmapshaper::ms_simplify(building_sf, keep = simplify_keep, keep_shapes = TRUE)

  # Compute area and perimeter
  building_sf$area <- as.numeric(sf::st_area(building_sf))
  building_sf$perim <- as.numeric(sf::st_length(building_sf))
  building_sf$perim_area_ratio <- building_sf$perim / building_sf$area

  # Filter polygons by area
  building_sf <- building_sf[building_sf$area > min_area & building_sf$area < max_area, , drop = FALSE]

  # Cast MULTIPOLYGON → POLYGON if needed
  building_sf <- sf::st_cast(building_sf, "POLYGON")

  # Transform CRS only if both have valid CRS
  if (!is.na(sf::st_crs(mask_shape)) && !is.na(sf::st_crs(building_sf))) {
    building_sf <- sf::st_transform(building_sf, sf::st_crs(mask_shape))
  }

  building_sf
}

