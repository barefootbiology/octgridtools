#' Create a binary slicer from a segmentation array
#'
#' These two functions create binary slicers from segmentation arrays by either
#' specifying a number range of values or by specifying values directly.
#'
#' @export
create_slicer_range <- function(seg_volume, from, to) {
  slicer <- (seg_volume >= from) & (seg_volume <= to) * 1

  slicer[slicer == 0] <- NA

  slicer
}
