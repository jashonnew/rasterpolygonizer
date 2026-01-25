#' Fill raster values using neighborhood-based ground estimation
#'
#' Applies a focal operation to fill NA values in a raster using
#' a clustering-based ground estimation method.
#'
#' @param x A `terra::SpatRaster`.
#' @param w Focal window size (odd integer).
#' @return A `terra::SpatRaster`.
#' @export
fill_ground_raster <- function(x, w = 15) {
  
  stopifnot(inherits(x, "SpatRaster"))
  stopifnot(w %% 2 == 1)
  
  terra::focal(
    x,
    w = w,
    fun = ground_fill_neighborhood
  )
}