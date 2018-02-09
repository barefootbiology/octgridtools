#' Create a circle
#'
#' Code based on:
#' http://gastonsanchez.com/blog/how-to/2012/06/17/PCA-in-R.html
#'
#' @export
#' @importFrom tibble tibble
generate_circle <- function(center = c(0, 0), radius = 1, npoints = 100) {
  tt <- seq(0, 2 * pi, length = npoints)
  xx <- center[1] + radius * cos(tt)
  yy <- center[2] + radius * sin(tt)

  tibble(x = xx, y = yy)
}
