#' Get the undefined values from a `segmentation` object
#'
#' Gets the undefined A-scans from a `segmentation` object.
#'
#' @export
#' @importFrom dplyr mutate
get_undefined <- function(segmentation) {
  segmentation$undefined_region %>%
    mutate(x = ascan_id, y = bscan_id) %>%
    mutate(is_defined = FALSE)
}
