#' Get the undefined values from a `segmentation` object as a matrix
#'
#' Returns the undefined A-scans from a `segmentation` object as a matrix.
#'
#' @export
#' @importFrom dplyr mutate full_join select if_else
#' @importFrom reshape2 melt acast
#' @importFrom rlang set_names
#' @importFrom magrittr %>%
get_undefined_matrix <- function(segmentation) {
  array(data = 1,
        dim = c(segmentation$info$size_x, segmentation$info$size_z)) %>%
    melt() %>%
    set_names(c("ascan_id", "bscan_id", "value")) %>%
    full_join(get_undefined(segmentation)) %>%
    mutate(is_defined = if_else(is.na(is_defined), 1, as.double(NA))) %>%
    select(ascan_id, bscan_id, is_defined) %>%
    acast(ascan_id ~ bscan_id, value.var = "is_defined")
}
