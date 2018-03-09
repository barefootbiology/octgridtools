# Produces an array in the shape of:
# dim(# ascans, # bscans, #surface_id)
iowa_segmentation_to_array <- function(segmentation) {
  segmentation$layers %>%
    select(ascan_id, bscan_id, surface_id, value) %>%
    reshape2::acast(formula = ascan_id ~ bscan_id ~ surface_id)
}
