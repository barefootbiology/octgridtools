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
                              cell_id = NA,
                              is_circle = FALSE) {

  # If radius_from == 0, we're dealing with a circle.
  # If radius_from > 0 and is_circle == FALSE, were dealing with a sector
  # If radius_from > 0 and is_circle == TRUE, we're dealing with a ring.

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

    if(is_circle) {
      # Repeating the first point before the hole ensures that
      # `ggplot2::geom_path` will correctly draw the hole without connecting
      # the inner and outer segments. See Dewey Dunnington's post for details:
      # https://apps.fishandwhistle.net/archives/1126
      sector <- bind_rows(sector, sector[1, ]) %>%
        mutate(is_hole = FALSE) %>%
        bind_rows(DA %>% mutate(is_hole = TRUE))
    } else {
      sector <- bind_rows(sector, DA) %>% mutate(is_hole = FALSE)
    }
  }

  sector %>%
    mutate(.cell_id = cell_id)
}
