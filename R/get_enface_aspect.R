#' Compute the proper aspect ratio of the en face image
#'
#' Computes the aspect ratio of an en face image from a volume object. Useful
#' for rendering en face images in `ggplot2`.
#'
#' @export
get_enface_aspect <- function(volume) {
  (volume$header$distance / volume$header$scale_y_slo) /
    (volume$header$scale_x / volume$header$scale_x_slo)
}
