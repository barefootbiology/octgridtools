#' Make sectors
#'
#' Make sectors
#'
#' @export
#' @importFrom dplyr mutate rowwise do ungroup
#' @importFrom magrittr %>%
make_grid_sectors <- function(grid_reg, center_x, center_y) {
  grid_reg %>%
    mutate(radius_from = radius_from,
           radius_to = radius_to) %>%
    mutate(angle_from = d2r(angle_from),
           angle_to = d2r(angle_to)) %>%
    rowwise() %>%
    do(sector_to_polygon(radius_from = .$radius_from, radius_to = .$radius_to,
                         angle_from = .$angle_from, angle_to = .$angle_to,
                         sector_id = .$etdrs_region,
                         center = c(center_x, center_y))) %>%
    ungroup() %>%
    return()
}
