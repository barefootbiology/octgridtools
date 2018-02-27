# Produces an array in the shape of:
# dim(# ascans, # bscans, #layer_y_order)
iowa_segmentation_to_array <- function(segmentation) {
  segmentation$layers %>% 
    select(ascan_id, bscan_id, layer_y_order, value) %>% 
    reshape2::acast(formula = ascan_id ~ bscan_id ~ layer_y_order) 
} 