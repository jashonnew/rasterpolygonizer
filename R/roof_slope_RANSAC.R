#' Estimate Roof Slope and Aspect Using Robust RANSAC Plane Fitting
#'
#' Fits a robust plane to raster elevation values within building polygons
#' using a RANSAC-style approach. Returns per-building slope, aspect, inlier
#' counts, and a stability metric derived from the top 5% of candidate fits.
#'
#' @param raster A `terra::SpatRaster` containing elevation values.
#' @param buildings A `terra::SpatVector` of building polygons. Must contain a `building_id` field.
#' @param n_iter Integer. Number of RANSAC iterations. Default is 500.
#' @param thresh Numeric. Residual threshold for inlier classification. Default is 0.1.
#' @param min_inliers Integer. Minimum number of inliers required to accept a plane. Default is 10.
#' @param quiet Logical. If FALSE (default), progress messages are printed.
#'
#' @return A list with:
#' \describe{
#'   \item{results_list}{Named list of per-building results.}
#'   \item{summary_table}{Data frame summarizing slope, aspect, inliers, and stability metrics.}
#' }
#'
#' @details
#' Slope is calculated as:
#' \deqn{atan(sqrt(a^2 + b^2)) * 180 / pi}
#'
#' Aspect is calculated as:
#' \deqn{(atan2(-a, -b) * 180 / pi) %% 360}
#'
#' Stability is computed as the 97.5th minus 2.5th percentile of slope
#' values across the top 5% of candidate fits ranked by inlier count.
#'
#' @export
roof_slope_RANSAC <- function(raster,
                                 buildings,
                                 n_iter = 500L,
                                 thresh = 0.1,
                                 min_inliers = 10L,
                                 quiet = FALSE) {

  # --- Input validation ---
  if (!inherits(raster, "SpatRaster")) {
    stop("`raster` must be a terra::SpatRaster.")
  }

  if (!inherits(buildings, "SpatVector")) {
    stop("`buildings` must be a terra::SpatVector.")
  }

  if (!"building_id" %in% names(buildings)) {
    stop("`buildings` must contain a 'building_id' column.")
  }

  results <- list()

  for (i in seq_len(terra::nrow(buildings))) {

    bldg <- buildings[i, ]
    id   <- bldg$building_id

    # --- Extract raster values ---
    ex <- terra::extract(raster, bldg, cells = TRUE, xy = TRUE)
    v  <- stats::na.omit(ex)

    if (nrow(v) < min_inliers) {
      if (!quiet) message("Building ", id, ": not enough points.")
      next
    }

    exclude_cols <- c("ID", "cell", "x", "y")
    z_col <- setdiff(names(v), exclude_cols)[1]

    xyz <- cbind(v$x, v$y, v[[z_col]])
    colnames(xyz) <- c("x", "y", "z")

    best_inliers <- NULL
    best_plane   <- NULL

    candidate_records <- vector("list", n_iter)
    n_candidates <- 0L

    for (j in seq_len(n_iter)) {

      idx <- sample.int(nrow(xyz), 3L)
      pts <- xyz[idx, , drop = FALSE]

      fit <- tryCatch(
        stats::lm(z ~ x + y, data = as.data.frame(pts)),
        error = function(e) NULL
      )

      if (is.null(fit)) next

      beta_tmp <- stats::coef(fit)
      if (any(is.na(beta_tmp))) next

      z_pred_tmp <- beta_tmp[1] +
        beta_tmp[2] * xyz[, 1] +
        beta_tmp[3] * xyz[, 2]

      residuals_tmp <- abs(xyz[, 3] - z_pred_tmp)

      valid_idx <- which(!is.na(residuals_tmp))
      residuals_tmp <- residuals_tmp[valid_idx]
      xyz_valid <- xyz[valid_idx, , drop = FALSE]

      inliers <- xyz_valid[residuals_tmp < thresh, , drop = FALSE]

      if (nrow(inliers) < min_inliers) next

      # --- Candidate qualifies ---
      a_tmp <- beta_tmp["x"]
      b_tmp <- beta_tmp["y"]

      slope_tmp <- atan(sqrt(a_tmp^2 + b_tmp^2)) * 180 / pi

      n_candidates <- n_candidates + 1L
      candidate_records[[n_candidates]] <- list(
        n_inliers = nrow(inliers),
        slope_deg = slope_tmp
      )

      if (is.null(best_inliers) ||
          nrow(inliers) > nrow(best_inliers)) {
        best_inliers <- inliers
        best_plane   <- beta_tmp
      }
    }

    if (is.null(best_plane)) {
      if (!quiet) message("Building ", id, ": no valid plane found.")
      next
    }

    candidate_records <- candidate_records[seq_len(n_candidates)]

    slope_spread <- NA_real_
    top5_count   <- NA_integer_

    if (n_candidates >= 1L) {

      inlier_counts <- vapply(
        candidate_records,
        `[[`,
        numeric(1),
        "n_inliers"
      )

      top_n   <- max(1L, ceiling(n_candidates * 0.05))
      top_idx <- order(inlier_counts, decreasing = TRUE)[seq_len(top_n)]

      top_slopes <- vapply(
        candidate_records[top_idx],
        `[[`,
        numeric(1),
        "slope_deg"
      )

      top5_count <- length(top_slopes)

      if (top5_count >= 2L) {
        slope_spread <- diff(
          stats::quantile(top_slopes, probs = c(0.025, 0.975))
        )
      }
    }

    # --- Final slope + aspect ---
    a <- best_plane["x"]
    b <- best_plane["y"]

    slope_deg  <- atan(sqrt(a^2 + b^2)) * 180 / pi
    aspect_deg <- (atan2(-a, -b) * 180 / pi) %% 360

    results[[as.character(id)]] <- list(
      building_id   = id,
      plane_coeff   = best_plane,
      slope_deg     = slope_deg,
      aspect_deg    = aspect_deg,
      n_inliers     = nrow(best_inliers),
      slope_spread  = slope_spread,
      top5_count    = top5_count,
      inliers       = best_inliers
    )

    if (!quiet) {
      message(
        "Processed building ", id,
        " (", nrow(best_inliers),
        " inliers | spread: ",
        round(slope_spread, 2),
        "degree | top5 n=", top5_count, ")"
      )
    }
  }

  summary_table <- do.call(
    rbind,
    lapply(results, function(x) {
      data.frame(
        building_id         = x$building_id,
        slope_deg           = x$slope_deg,
        aspect_deg          = x$aspect_deg,
        n_inliers           = x$n_inliers,
        slope_range        = x$slope_spread,
        top_5_percent_count = x$top5_count,
        stringsAsFactors = FALSE
      )
    })
  )

  rownames(summary_table) <- NULL

  list(
    results_list  = results,
    summary_table = summary_table
  )
}
