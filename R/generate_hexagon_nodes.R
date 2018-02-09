#' Generate hexagon nodes
#'
#' Give a center point, radius, and rotation, return the line segments
#' for a hexagon
#'
#' @export
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
generate_hexagon_nodes <- function(center_x,
                                   center_y,
                                   radius = 1,
                                   rotation = 0) {
  angle_increment <- (pi * 1 / 3)

  tibble(center_x = center_x, center_y = center_y, .node_id = 0:6) %>%
    mutate(angle = angle_increment * .node_id) %>%
    mutate(opp = sin(angle + rotation) * radius) %>%
    mutate(adj = cos(angle + rotation) * radius) %>%
    mutate(x = center_x + adj, y = center_y + opp) %>%
    mutate(.node_id = .node_id + as.integer(1))
}
