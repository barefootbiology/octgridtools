get_undefined_matrix <- function(segmentation) {
  # segmentation$undefined_region %>%
  #   mutate(x = ascan_id, y = bscan_id) %>%
  #   mutate(is_defined = FALSE)

  array(data = 1,
        dim = c(segmentation$info$size_x, segmentation$info$size_z)) %>%
    reshape2::melt() %>%
    set_names(c("ascan_id", "bscan_id", "value")) %>%
    full_join(get_undefined(segmentation)) %>%
    mutate(is_defined = if_else(is.na(is_defined), 1, as.double(NA))) %>%
    select(ascan_id, bscan_id, is_defined) %>%
    reshape2::acast(ascan_id ~ bscan_id, value.var = "is_defined")
}
