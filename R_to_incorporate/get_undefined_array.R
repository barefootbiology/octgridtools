get_undefined_array <- function(segmentation) {
  # array(data = 1,
  #       dim = c(segmentation$info$size_x,
  #               segmentation$info$size_z,
  #               segmentation$info$size_y)) %>%
  #   reshape2::melt() %>%
  #   set_names(c("ascan_id", "bscan_id", "value")) %>%
  #   full_join(get_undefined(segmentation)) %>%
  #   mutate(is_defined = if_else(is.na(is_defined), 1, as.double(NA))) %>%
  #   select(ascan_id, bscan_id, is_defined) %>%
  #   reshape2::acast(ascan_id ~ bscan_id, value.var = "is_defined")

  get_undefined_matrix(segmentation) %>%
    as.vector() %>%
    rep(times = segmentation$info$size_y) %>%
    array(dim = c(segmentation$info$size_x,
                  segmentation$info$size_z,
                  segmentation$info$size_y))
}
