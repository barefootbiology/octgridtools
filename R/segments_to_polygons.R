#' Convert segments to polygons
#'
#' Takes a data.frame of segments grouped by polygon and returns a SpatialPolygonsDataFrame
#'
#' @export
#' @importFrom dplyr group_by_ do ungroup select_
#' @importFrom sp coordinates Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @importFrom magrittr %>%
segments_to_polygons <- function(segments_by_group, group, data, x = "x", y = "y") {
  region_segments <-
    segments_by_group %>%
    group_by_(group) %>%
    do(.repeat_first(.)) %>%
    ungroup()

  region_segments %>%
    by(
      region_segments[[group]],
      function(z) z %>%
          select_(x, y) %>%
          as.data.frame() %>%
          as.matrix() %>%
          coordinates() %>%
          Polygon(hole = FALSE) %>%
          list() %>%
          Polygons(ID = z[1, group])
    ) %>%
    unlist() %>%
    SpatialPolygons() %>%
    SpatialPolygonsDataFrame(data = data)
}
