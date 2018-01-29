#' Convert x and y columns in a data.frame to SpatialPoints
#'
#' Takes a data.frame containing x and y coordinates and returns a SpatialPoints object
#'
#' @export
#' @importFrom sp coordinates SpatialPoints
#' @importFrom magrittr %>%
make_points <- function(df, x = "x", y = "y") {
  df %>%
    select_(x, y) %>%
    as.data.frame %>%
    as.matrix() %>%
    coordinates() %>%
    SpatialPoints() %>%
    return()
}
