# Compute the proper aspect ratio of the en face image
get_enface_aspect <- function(volume) {
  (volume$header$distance / volume$header$scale_y_slo) /
    (volume$header$scale_x / volume$header$scale_x_slo) 
}