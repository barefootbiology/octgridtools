#' Determine the center coordinates of a `grid center` object in SLO space
#'
#' Determines the center coordinate of a `grid center` object in SLO space.
#'
#' @export
#' @importFrom scales rescale
#' @importFrom magrittr %>%
get_slo_center <- function(volume, grid_center) {
  # Determine the center coordinates of grid_center in the SLO
  # space.

  # Find the coordinates on the b-scan
  center_bscan <- volume$bscan_headers %>%
    filter(bscan_id == grid_center$center$z[[1]])

  # A-scan
  x <- rescale(
    x = grid_center$center$x[[1]],
    # A-scan space
    from = c(1, volume$header$size_x),
    # SLO space
    to = c(center_bscan$start_x_pixels,
           center_bscan$end_x_pixels))

  # B-scan
  y <- rescale(
    x = grid_center$center$z[[1]],
    # A-scan space
    from = c(1, volume$header$num_bscans),
    # SLO space
    to = c(center_bscan$start_y_pixels,
           center_bscan$end_y_pixels))

  c(x = x, y = y)
}
