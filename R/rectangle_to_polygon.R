#' Convert rectangular min and max points to a polygon
#'
#' Convert two points representing a rectangle to four points representing a
#' polygon.
#'
#' @export
#' @importFrom tibble tibble
rectangle_to_polygon <- function(xmin, xmax, ymin, ymax) {
  tibble(
    x = c(xmin, xmin, xmax, xmax),
    y = c(ymin, ymax, ymax, ymin),
    .node_id = 1:4
  )
}
