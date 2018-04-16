#' Create a binary slicer from a segmentation array
#'
#' These two functions create binary slicers from segmentation arrays by either
#' specifying a number range of values or by specifying values directly.
#'
#' @export
create_slicer_subset <- function(seg_volume, values) {
  slicer <- array(seg_volume %in% values, dim = dim(seg_volume)) * 1

  slicer[slicer == 0] <- NA

  slicer
}
