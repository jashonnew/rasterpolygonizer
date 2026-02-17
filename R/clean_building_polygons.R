# Internal helper — not exported
explode_rings <- function(geom) {
  lapply(seq_along(geom), function(i) sf::st_polygon(list(geom[[i]])))
}

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

  if (!is.null(mask_shape)) {
    r_crop   <- terra::crop(closed_edges, mask_shape)
    r_masked <- terra::mask(r_crop, mask_shape)
  } else {
    r_masked <- closed_edges
  }

  # Invert: polygonize the FALSE (purple/interior) regions.
  # The yellow edge loops enclose purple interiors — those interiors
  # are the actual building footprints we want as polygons.
  r_interior <- !r_masked

  building_polys <- terra::as.polygons(r_interior, dissolve = TRUE)
  building_sf    <- sf::st_as_sf(building_polys)

  # Keep only the interior (TRUE after inversion) class
  # as.polygons will produce two rows: one for FALSE regions, one for TRUE
  col <- names(building_sf)[1]
  building_sf <- building_sf[building_sf[[col]] == 1, ]

  # Shrink polygons
  building_sf <- sf::st_buffer(building_sf, dist = shrink_dist)
  building_sf <- sf::st_make_valid(building_sf)

  # Simplify
  building_sf <- sf::st_simplify(building_sf, dTolerance = simplify_tol, preserveTopology = TRUE)

  # Explode all rings into individual polygons
  crs <- sf::st_crs(building_sf)

  all_polys <- unlist(lapply(sf::st_geometry(building_sf), function(geom) {
    if (inherits(geom, "MULTIPOLYGON")) {
      unlist(lapply(seq_along(geom), function(i) {
        explode_rings(geom[[i]])
      }), recursive = FALSE)
    } else {
      explode_rings(geom)
    }
  }), recursive = FALSE)

  building_sf <- sf::st_sf(
    geometry = sf::st_sfc(all_polys, crs = crs)
  )

  building_sf <- sf::st_make_valid(building_sf)
  building_sf$building_id <- seq_len(nrow(building_sf))

  building_sf <- building_sf[
    !sf::st_is_empty(building_sf) &
      sf::st_is_valid(building_sf),
  ]

  building_sf$area  <- as.numeric(sf::st_area(building_sf))
  building_sf$perim <- as.numeric(sf::st_length(building_sf))
  building_sf$perim_area_ratio <- building_sf$perim / building_sf$area

  building_sf$vertices <- sapply(sf::st_geometry(building_sf), function(x) {
    sum(sapply(x, function(polygon) nrow(polygon)))
  })

  # Area filter now cleanly separates buildings from background:
  # - too small: noise/gaps between edge pixels
  # - too large: the surrounding ground/background region
  building_sf <- building_sf[
    building_sf$area > min_area & building_sf$area < max_area, ,
    drop = FALSE
  ]

  if (!is.null(mask_shape) &&
      !is.na(sf::st_crs(mask_shape)) &&
      !is.na(sf::st_crs(building_sf))) {
    building_sf <- sf::st_transform(building_sf, sf::st_crs(mask_shape))
  }

  building_sf
}
