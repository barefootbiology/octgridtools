#' Convert a data.frame to a SpatialPoints object
#'
#' Convert a data.frame to a SpatialPoints object
#'
#' @export
#' @importFrom dplyr select
#' @importFrom sp SpatialPoints coordinates
tibble_to_spatialpoints <- function(tbl) {
  tbl %>%
    select(x, y) %>%
    as.data.frame %>%
    as.matrix() %>%
    coordinates() %>%
    SpatialPoints() %>%
    return()
}
