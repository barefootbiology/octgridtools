#' Apply spline-based correction on an array containing Heidelberg B-scans
#'
#' Applys a spline-based correction to an array. Correction is based modeled on
#' the relationship between uncorrected values and the PNGs exported from
#' Heidelberg Explorer.
#'
#' @export
#' @importFrom dplyr if_else
#' @importForm heyexr spline_correction
spline_correction_array <- function(a) {
  if_else(is.na(as.vector(a)), 0, a) %>%
    spline_correction() %>%
    array(dim = dim(a))
}
