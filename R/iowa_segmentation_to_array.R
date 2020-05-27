#' Convert Iowa segmentation data.frame to array
#'
#' Convert Iowa segmentation data.frame to an array.
#' Produces an array in the shape of:
#' dim(# ascans, # bscans, # surface_id)
#'
#' @export
#' @importFrom dplyr select
#' @importFrom reshape2 acast
iowa_segmentation_to_array <- function(segmentation, mask_undefined = FALSE) {
  seg_layers <- segmentation$layers

  seg_undefined <-
    segmentation$undefined_region %>%
    mutate(is_defined = FALSE)

  if(mask_undefined) {
    seg_layers <-
      seg_layers %>%
      left_join(seg_undefined) %>%
      mutate(is_defined = if_else(is.na(is_defined), TRUE, FALSE)) %>%
      mutate(value = if_else(is_defined, value, as.numeric(NA)))
  }

  seg_layers %>%
    select(ascan_id, bscan_id, surface_id, value) %>%
    acast(formula = ascan_id ~ bscan_id ~ surface_id)
}
