#' Generate a grid of rectangles
#'
#' Generates a grid of rectangles around a center point
#'
#' @export
#' @importFrom dplyr mutate
#' @importFrom scales rescale
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom purrrlyr by_row
generate_grid_rectangles <- function(center_x, center_y, nrow, ncol, width) {
  tibble(
    .cell_id = 1:(nrow * ncol),
    row_id = rep(1:nrow, each = ncol),
    col_id = rep(1:ncol, times = nrow)
  ) %>%
    mutate(
      x = col_id * width,
      y = row_id * width
    ) %>%
    mutate(.x = rescale(
      x = x,
      to = c(
        (center_x - (width * (ncol - 1)) / 2),
        (center_x + (width * (ncol - 1)) / 2)
      )
    )) %>%
    mutate(.y = rescale(
      x = y,
      to = c(
        (center_y - (width * (ncol - 1)) / 2),
        (center_y + (width * (ncol - 1)) / 2)
      )
    )) %>%
    mutate(
      xmin = .x - (width / 2),
      ymin = .y - (width / 2),
      xmax = .x + (width / 2),
      ymax = .y + (width / 2)
    ) %>%
    select(-x, -y) %>%
    by_row(
      ~rectangle_to_polygon(.x$xmin, .x$xmax, .x$ymin, .x$ymax),
      .collate = "rows"
    ) %>%
    group_by(.cell_id) %>%
    mutate(.node_id = 1:n()) %>%
    ungroup() %>%
    select(.cell_id, .node_id, x, y, row_id, col_id)
}
