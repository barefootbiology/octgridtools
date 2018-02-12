#' Find points in regions
#'
#' Determine which points appear in which regions.
#'
#' @importFrom sp over
#' @importFrom dplyr bind_cols as_tibble
#' @importFrom magrittr %>%
find_sppoints_in_sppolygons <- function(pts, plygns, pts_attributes) {
  sp::over(
    pts,
    plygns
  ) %>%
    bind_cols(pts_attributes) %>%
    as_tibble()
}
