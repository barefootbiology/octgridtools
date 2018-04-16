#' Replace elements based on a function
#'
#' Replaces elements based on a function which returns a logical condition.
#'
#' @export
#' @importFrom dplyr if_else
find_replace <- function(.x, replacement, find_fun, ...) {
  if_else(find_fun(.x, ...), replacement, x)
}
