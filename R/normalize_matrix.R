#' Normalize values in a matrix
#'
#' Normalizes values in a matrix to lie between 0-1. NOTE: Perhaps this should
#' be termed `rescale_matrix`.
#'
#' @export
#' @importFrom magrittr %>%
normalize_matrix <- function(m) {
  dim_m <- dim(m)

  m %>%
    as.vector() %>%
    (function(i) (i - min(i, na.rm = TRUE)) / max(i - min(i, na.rm = TRUE),
                                                  na.rm = TRUE)) %>%
    array(dim = dim_m)
}
