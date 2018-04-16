#' Replace NA values in a vector
#'
#' Replaces NA values in a vector.
#'
#' @export
#' @importFrom dplyr if_else
na_replace <- function(x, replacement) {
  #find_replace(.x = x, replacement = replacement, find_fun = is.na)
  if_else(is.na(x), replacement, x)
}
