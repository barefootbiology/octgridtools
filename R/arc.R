#' Create an arc
#'
#' Create an arc.
#'
#' @export
arc <- function(from, to, radius, center = c(0, 0), n_points = 100) {
  tt <- seq(from = from, to = to, length = n_points)
  xx <- center[1] + radius * cos(tt)
  yy <- center[2] + radius * sin(tt)

  data.frame(x = xx, y = yy, angle = tt, id = 1:n_points)
}
