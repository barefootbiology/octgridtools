#' List files matching a pattern
#'
#' Handy wrapper for recursively listing files.
#'
#' @export
list_files <- function(path, pattern) {
  list.files(
    path = path,
    pattern = pattern,
    ignore.case = TRUE,
    recursive = TRUE,
    full.names = TRUE
  ) %>%
    sort()
}
