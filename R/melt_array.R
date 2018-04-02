#' Reshape an array to a tibble
#'
#' Reshapes an array to a tibble, renames the columns.
#'
#' @export
#' @importFrom reshape2 melt
#' @importFrom purrr set_names
#' @importFrom tibble as_tibble
melt_array <- function(x, nm) {
  reshape2::melt(data = x) %>%
    set_names(nm = nm) %>%
    as_tibble()
}
