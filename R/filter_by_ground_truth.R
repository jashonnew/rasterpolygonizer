#' Filter candidate polygons by ground truth containment
#'
#' Filters candidate building polygons using a ground-truth polygon set.
#' Keeps candidate polygons whose area is sufficiently contained
#' within intersecting ground-truth polygons.
#'
#' @param candidates An sf polygon object (e.g., extracted buildings).
#' @param truth An sf polygon object representing ground truth.
#' @param threshold Minimum containment ratio (default 0.75).
#'
#' @return Filtered sf object containing only accepted polygons.
#' @export
filter_by_ground_truth <- function(
    candidates,
    truth,
    threshold = 0.75
) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' required for this function.")
  }

  if (!inherits(candidates, "sf") || !inherits(truth, "sf")) {
    stop("Both candidates and truth must be sf objects.")
  }

  # ---- CRS handling ----
  crs_candidates <- sf::st_crs(candidates)
  crs_truth <- sf::st_crs(truth)

  if (is.na(crs_candidates)) {
    stop("candidates has missing CRS")
  }

  if (is.na(crs_truth))
    stop("truth has missing CRS")

  if (!sf::st_crs(candidates) == sf::st_crs(truth)) {
    warning("CRS mismatch detected. Transforming truth to match candidates CRS.")
    truth <- sf::st_transform(truth, crs_candidates)
  }

  # find intersections
  int_list <- sf::st_intersects(candidates, truth)

  # build origin-match table
  intersections <- do.call(
    rbind,
    lapply(seq_along(int_list), function(i) {
      if (length(int_list[[i]]) == 0) return(NULL)
      data.frame(origin = i, match = int_list[[i]])
    })
  )

  if (is.null(intersections) || nrow(intersections) == 0) {
    return(candidates[0, ])  # return empty sf
  }

  # compute containment ratios
  containment_results <- lapply(seq_len(nrow(intersections)), function(i) {

    cand <- candidates[intersections$origin[i], ]
    clip <- truth[intersections$match[i], ]

    inter <- sf::st_intersection(cand, clip)

    intersection_area <- if (nrow(inter) > 0)
      as.numeric(sf::st_area(inter))
    else 0

    candidate_area <- as.numeric(sf::st_area(cand))

    data.frame(
      origin = intersections$origin[i],
      containment_ratio =
        if (candidate_area > 0)
          intersection_area / candidate_area
      else 0
    )
  })

  containment_df <- do.call(rbind, containment_results)

  keep_origins <- unique(
    containment_df$origin[containment_df$containment_ratio >= threshold]
  )

  candidates[keep_origins, , drop = FALSE]
}
