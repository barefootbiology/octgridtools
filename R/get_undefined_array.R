#' Get the undefined values from a `segmentation` object as an array
#'
#' Returns the undefined A-scans from a `segmentation` object as an array.
#'
#' @export
#' @importFrom dplyr mutate
get_undefined_array <- function(segmentation) {
  get_undefined_matrix(segmentation) %>%
    as.vector() %>%
    rep(times = segmentation$info$size_y) %>%
    array(dim = c(segmentation$info$size_x,
                  segmentation$info$size_z,
                  segmentation$info$size_y))
}
