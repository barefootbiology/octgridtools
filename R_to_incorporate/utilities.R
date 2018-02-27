source_dir <- function(.path) {
  list.files(path = .path, 
             pattern = ".R",
             full.names = TRUE) %>%
    purrr::walk(~source(file = .x))
}

get_sample_id <- function(x) {
  # Get the directory name just up from the XML file.
  # This will be the OCT file base that I use as a sample_id
  dirname(path = x) %>%
    basename() %>%
    return()
}

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