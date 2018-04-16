#' Compute thickness of a segmentation array
#'
#' Computes the thickness of a 2D segmentation array by subtracking the z values
#' of adjacent layers. NOTE: This might give unreliable results if layers
#' are allowed to intersect (i.e., after manual correction).
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom reshape2 melt
#' @importFrom tibble as_tibble
#' @importFrom rlang set_names
#' @importFrom dplyr full_join mutate select if_else
compute_thickness <- function(seg_array,
                              na_undefined = FALSE,
                              scale_value = FALSE) {
  thickness <- seg_array %>%
    apply(c(1,2), (function(x) x[2] - x[1])) %>%
    reshape2::melt() %>%
    as_tibble() %>%
    set_names(c("x", "y", "thickness"))

  if(na_undefined != FALSE) {
    thickness <-
      thickness %>%
      full_join(na_undefined) %>%
      mutate(is_defined = if_else(is.na(is_defined), TRUE, is_defined)) %>%
      mutate(thickness = if_else(is_defined, thickness, as.numeric(NA))) %>%
      select(-is_defined, -bscan_id, -ascan_id)
  }

  if(scale_value != FALSE) {
    thickness <-
      thickness %>%
      mutate(thickness_scaled = thickness * scale_value)
  }

  thickness
}
