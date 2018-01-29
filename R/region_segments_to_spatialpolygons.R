#' Convert grid regions to SpatialPolygons
#'
#' Convert grid regions to SpatialPolygons
#'
#' @export
#' @importFrom dplyr group_by do ungroup select distinct
#' @importFrom sp coordinates Polygon Polygons SpatialPolygons
#' @importFrom sp SpatialPolygonsDataFrame
#' @importFrom magrittr %>%
region_segments_to_spatialpolygons <- function(reg_seg) {
  # In order to make SpatialPolygons, we need to close the polygon by
  # repeating the first point.
  reg_seg_rep <- reg_seg %>%
    group_by(sector_id) %>%
    do(.repeat_first(.)) %>%
    ungroup()

  reg_seg_rep %>%
    by(reg_seg_rep$sector_id,
       function(x) x %>% select(x, y) %>%
         as.data.frame() %>%
         as.matrix() %>%
         coordinates() %>%
         Polygon(hole = FALSE) %>%
         list %>%
         Polygons(ID = x[1, "sector_id"])) %>%
    unlist() %>%
    SpatialPolygons() %>%
    SpatialPolygonsDataFrame(reg_seg %>%
                               select(sector_id) %>%
                               distinct() %>%
                               as.data.frame())

}
