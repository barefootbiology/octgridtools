#' Make sectors
#'
#' Make sectors
#'
#' @export
#' @importFrom dplyr mutate rowwise do ungroup
#' @importFrom magrittr %>%
generate_grid_sectors <- function(grid_reg, center_x, center_y) {
  grid_reg %>%
    mutate(
      radius_from = radius_from,
      radius_to = radius_to
    ) %>%
    mutate(
      angle_from = degree_to_radian(angle_from),
      angle_to = degree_to_radian(angle_to)
    ) %>%
    rowwise() %>%
    do(sector_to_polygon(
      radius_from = .$radius_from, radius_to = .$radius_to,
      angle_from = .$angle_from, angle_to = .$angle_to,
      cell_id = .$.cell_id,
      center = c(center_x, center_y),
      is_circle = .$is_circle
    )) %>%
    ungroup() %>%
    rename(.node_id = id) %>%
    select(.cell_id, .node_id, x, y, is_hole) %>%
    mutate(.group_id = paste(.cell_id, as.numeric(is_hole) + 1, sep = "_"))
}
