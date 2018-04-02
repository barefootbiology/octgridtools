#' Convert Iowa segmentation data.frame to array
#'
#' Convert Iowa segmentation data.frame to an array.
#' Produces an array in the shape of:
#' dim(# ascans, # bscans, #surface_id)
#'
#' @export
#' @importFrom dplyr select
#' @importFrom reshape2 acast
iowa_segmentation_to_array <- function(segmentation) {
  segmentation$layers %>%
    select(ascan_id, bscan_id, surface_id, value) %>%
    acast(formula = ascan_id ~ bscan_id ~ surface_id)
}
