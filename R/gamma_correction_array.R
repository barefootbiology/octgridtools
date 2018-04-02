#' Apply gamma correction on an array of data
#'
#' Applys a gamma correction to an array. Default is the value suggested by
#' Heidelberg for rendering B-scans in VOL files.
#'
#' @export
#' @importFrom dplyr if_else
gamma_correction_array <- function(a, gamma = 1/4) {
  if_else(is.na(as.vector(a)), 0, a) %>%
    (function(x) x^gamma) %>%
    array(dim = dim(a))
}
