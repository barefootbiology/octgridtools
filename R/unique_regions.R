#' Identify the unique region IDs in a segmentation volume
#'
#' Identifies the unique region IDs in a segmentation volume.
#'
#' @export
unique_regions <- function(seg_volume) {
  seg_volume %>%
    as.vector() %>%
    unique() %>%
    sort()
}
