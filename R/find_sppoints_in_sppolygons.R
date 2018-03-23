#' Find points in regions
#'
#' Determine which points appear in which regions.
#'
#' @importFrom sp over
#' @importFrom dplyr bind_cols as_tibble
#' @importFrom magrittr %>%
find_sppoints_in_sppolygons <- function(pts, plygns, pts_attributes = NULL) {

  result <- sp::over(
    pts,
    plygns
  )

  if(!is.null(pts_attributes)) {
    result <- bind_cols(result, pts_attributes)
  }

  as_tibble(result)
}
