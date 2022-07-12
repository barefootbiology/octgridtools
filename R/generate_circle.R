#' Create a circle
#'
#'
#' @export
generate_circle <- function(center = c(0, 0), radius = 1, n = 100) {
  angle <- seq(0, 2 * pi, length = n)
  x <- center[1] + radius * cos(angle)
  y <- center[2] + radius * sin(angle)

  data.frame(x = x, y = y)
}
