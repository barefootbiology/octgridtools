#' Find points in regions
#'
#' Determine which points appear in which regions.
#'
#' @export
#' @importFrom sp over
#' @importFrom dplyr bind_cols as_tibble
#' @importFrom magrittr %>%
find_points_in_regions <- function(pts, rgns, pts_attributes) {
  sp::over(
    pts,
    rgns
  ) %>%
    bind_cols(pts_attributes) %>%
    as_tibble()
}
