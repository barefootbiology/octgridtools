#' Construct a sector or circle as a polygon
#'
#' Return a data frame of points that can be plotted as a polygon
#'
#' @export
#' @importFrom dplyr mutate bind_rows
#' @importFrom magrittr %>%
sector_to_polygon <- function(radius_from, radius_to,
                              angle_from, angle_to,
                              center = c(0, 0),
                              n_points = 100,
                              sector_id = NA,
                              is_circle = FALSE) {

  # If the arc passes through 360,
  # adjust the angle_to accordingly
  if (angle_from > angle_to) {
    angle_to <- angle_to + 2 * pi
  }

  # Corners (counterclockwise): A, B, C, D
  # A -> B: segment
  # B -> C: arc
  # C -> D: segment
  # D -> A: arc

  # A <- center[1] + radius_from * cos(angle_from)
  BC <- generate_arc(
    from = angle_from, to = angle_to,
    radius = radius_to,
    center = center, n_points = n_points
  ) %>%
    mutate(segment_id = "BC")

  sector <- BC

  if (radius_from > 0) {
    DA <- generate_arc(
      from = angle_to, to = angle_from,
      radius = radius_from,
      center = center, n_points = n_points
    ) %>%
      mutate(segment_id = "DA")

    sector <- bind_rows(sector, DA)
  }

  sector %>%
    mutate(sector_id = sector_id)
}
