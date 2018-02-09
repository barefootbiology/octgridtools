#' Generate a grid of regular hexagons
#'
#' Generate a grid of regular hexagons
#'
#' @export
#' @importFrom purrrlyr by_row
#' @importFrom dplyr select
generate_grid_hexagons <- function(nrow, ncol, radius) {
  generate_hexagon_centers(nrow = nrow, ncol = ncol, radius = radius) %>%
    purrrlyr::by_row(
      ~generate_hexagon_segments(
        .x$center_x, .x$center_y,
        radius = 6 / 11,
        rotation = 0
      ),
      .collate = "rows"
    ) %>%
    select(.cell_id, .node_id, x, y, row_id, col_id)
}
