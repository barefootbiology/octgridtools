#' Generate hexagon segments
#'
#' Generates segments corresponding to hexagonal shapes.
#'
#' @export
#' @importFrom dplyr select mutate filter
#' @importFrom magrittr %>%
generate_hexagon_segments <- function(center_x, center_y, radius=1, rotation=0) {
  generate_hexagon_nodes(
    center_x = center_x,
    center_y = center_y,
    radius = radius,
    rotation = rotation
  ) %>%
    select(center_x, center_y, node_id, x, y) %>%
    mutate(x_end = lead(x), y_end = lead(y)) %>%
    filter(!is.na(x_end))
}
