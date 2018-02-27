get_undefined <- function(segmentation) {
  segmentation$undefined_region %>% 
    mutate(x = ascan_id, y = bscan_id) %>% 
    mutate(is_defined = FALSE)
}