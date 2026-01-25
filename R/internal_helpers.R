#' Apply 2D convolution on a numeric vector
#'
#' Internal helper for Sobel kernels.
#'
#' @param vals Numeric vector (from focal neighborhood)
#' @param kernel_vec Numeric vector (flattened kernel)
#' @return Numeric single value
#' @keywords internal
conv_with_kernel <- function(vals, kernel_vec) {
  mask <- !is.na(vals)
  if (!any(mask)) return(NA_real_)
  num <- sum(vals[mask] * kernel_vec[mask])
  denom <- sum(abs(kernel_vec[mask]))
  if (denom == 0) return(NA_real_)
  num / denom
}

