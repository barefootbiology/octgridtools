#' TASK: Test this slope function on VOL files with known rotated volume scans.
#'
#' Return the volume bounding box in pixel coordinates, adjusted for the voxel
#' "size."
#'
#' @keywords internal
get_volume_bbox_in_slo <- function(volume) {
  # This will only work for volume scans!
  # NOTE: I think it will also work for linear scans, but I need to find out.
  if(!(volume$header$scan_pattern %in% c(1, 3, 4))) {
    error("get_volume_angle_in_slo only works for linear and volume scans.")
  }

  dx0 <- (volume$header$scale_x / volume$header$scale_x_slo) / 2

  dy0 <- (volume$header$distance / volume$header$scale_y_slo) / 2

  # TASK: Work out how to compensate for rotation.
  # NOTE: I suspect rotation is always 0 when we're dealing with registered
  #       OCTs. In a registered OCT, the SLO can get rotated, but the b-scans
  #       don't. In an unregistered OCT, the b-scans get rotated, but the SLO
  #       does not.

  # theta <- (0.5 * pi) - get_volume_angle_in_slo(volume = volume)

  # # Compensate for rotation of the b-scans
  # dvector <- c(-1 * dx0, -1 * dy0,  -1 * dx0, dy0,  dx0, dy0,  dx0, -1 * dy0)
  #
  # dmatrix0 <- matrix(dvector, ncol = 4)
  #
  # affine <- matrix(c(cos(theta), sin(theta), -1 * sin(theta), cos(theta)),
  #                  byrow = TRUE, ncol = 2)
  #
  # dmatrix <- affine %*% dmatrix0

  dx <- dx0
  dy <- dy0

  list(
    xmin = min(c(volume$bscan_headers$start_x_pixels,
                 volume$bscan_headers$end_x_pixels)) - dx,
    xmax = max(c(volume$bscan_headers$start_x_pixels,
                 volume$bscan_headers$end_x_pixels)) + dx,
    ymin = min(c(volume$bscan_headers$start_y_pixels,
                 volume$bscan_headers$end_y_pixels)) - dy,
    ymax = max(c(volume$bscan_headers$start_y_pixels,
                 volume$bscan_headers$end_y_pixels)) + dy
  )
}
