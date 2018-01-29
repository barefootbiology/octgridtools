#' Generate hexagon nodes
#'
#' Give a center point, radius, and rotation, return the line segments
#' for a hexagon
#'
#' @export
#' @importFrom dplyr tbl_df mutate
generate_hexagon_nodes = function(center_x, center_y, radius = 1, rotation = 0) {
  angle_increment = (pi * 1 / 3)

  data.frame(center_x, center_y, node_id = 0:6) %>%
    tbl_df %>%
    mutate(angle = angle_increment * node_id) %>%
    mutate(opp = sin(angle + rotation) * radius) %>%
    mutate(adj = cos(angle + rotation) * radius) %>%
    mutate(x = center_x + adj, y = center_y + opp) %>%
    return()
}
