#' Estimate ground value from a focal neighborhood
#'
#' Given a vector of neighboring raster values (including the center pixel),
#' returns a representative ground value using clustering-based heuristics.
#'
#' @param neighbors Numeric vector of focal window values.
#' @return A single numeric value or NA.
#' @keywords internal
ground_fill_neighborhood <- function(neighbors) {
  
  center_pixel <- neighbors[ceiling(length(neighbors) / 2)]
  
  if (!is.na(center_pixel)) {
    return(center_pixel)
  }
  
  neighbors_no_NA <- neighbors[!is.na(neighbors)]
  
  if (length(neighbors_no_NA) == 0) {
    return(NA_real_)
  }
  
  n_sort <- sort(neighbors_no_NA)
  n_unique <- length(unique(n_sort))
  k <- min(3, n_unique, length(n_sort))
  
  if (k < 2) {
    return(NA_real_)
  }
  
  km <- tryCatch(
    stats::kmeans(n_sort, centers = k, iter.max = 50),
    error = function(e) {
      diffs <- diff(n_sort)
      if (length(diffs) == 0) return(NA_real_)
      
      max_index <- which.max(diffs)
      if (length(max_index) == 0) return(stats::median(n_sort))
      
      lower_group <- n_sort[(max_index + 1):length(n_sort)]
      if (length(lower_group) == 0) return(NA_real_)
      
      return(stats::median(lower_group))
    }
  )
  
  if (!is.list(km) || is.null(km$cluster)) {
    return(km)
  }
  
  cluster_means <- tapply(n_sort, km$cluster, mean)
  lowest_cluster <- which.min(cluster_means)
  ground_values <- n_sort[km$cluster == lowest_cluster]
  
  stats::median(ground_values)
}
