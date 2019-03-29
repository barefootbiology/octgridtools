#' Return angle of B-scans in radians
#'
#' Returns the angle of B-scans in a VOL file.
#' @keywords internal
get_volume_angle_in_slo <- function(volume) {
  # This function assumes that all the b-scans are on the same angle.
  # This will only work for volume scans!
  if(!(volume$header$scan_pattern %in% c(1, 3, 4))) {
    error("get_volume_angle_in_slo only works for linear and volume scans.")
  }

  x0 <- as.numeric(volume$bscan_headers[1, "start_x_pixels"])
  y0 <- as.numeric(volume$bscan_headers[1, "start_y_pixels"])

  x1 <- as.numeric(volume$bscan_headers[1, "end_x_pixels"])
  y1 <- as.numeric(volume$bscan_headers[1, "end_y_pixels"])

  slope <- (y1 - y0) / (x1 - x0)

  angle <- atan(slope)

  if(slope < 0) (2*pi - angle) else angle
}
