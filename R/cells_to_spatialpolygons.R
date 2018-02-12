#' Convert grid regions to SpatialPolygons
#'
#' Convert grid regions to SpatialPolygons
#'
#' @export
#' @importFrom dplyr group_by do ungroup select distinct
#' @importFrom sp coordinates Polygon Polygons SpatialPolygons
#' @importFrom sp SpatialPolygonsDataFrame
#' @importFrom magrittr %>%
cells_to_spatialpolygons <- function(grid) {
  # In order to make SpatialPolygons, we need to close the polygon by
  # repeating the first point.
  reg_seg_rep <- grid %>%
    group_by(.cell_id) %>%
    do(.repeat_first(.)) %>%
    ungroup()

  reg_seg_rep %>%
    by(
      reg_seg_rep$.cell_id,
      function(x) x %>%
          select(x, y) %>%
          as.data.frame() %>%
          as.matrix() %>%
          coordinates() %>%
          Polygon(hole = FALSE) %>%
          list() %>%
          Polygons(ID = x[1, ".cell_id"])
    ) %>%
    unlist() %>%
    SpatialPolygons() %>%
    SpatialPolygonsDataFrame(grid %>%
      select(.cell_id) %>%
      distinct() %>%
      as.data.frame())
}
