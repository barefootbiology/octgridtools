#' Source all R scripts within a directory.
#'
#' Sources all R scripts within a specified directory.
#'
#' @export
#' @importFrom purrr walk
source_dir <- function(.path) {
  list.files(path = .path,
             pattern = ".R",
             full.names = TRUE,
             ignore.case = TRUE) %>%
    walk(~source(file = .x))
}
