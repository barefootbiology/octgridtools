#' Given an rows, columns, and radius, determine the grid of hexagon centers
#'
#' Code adapted from:
#' http://hacksociety.net/Thread-Tutorial-Creating-a-hexagonal-grid-for-games-C
#' which doesn't seem to be on the Internet anymore.
#' @keywords internal
.calc_y <- function(row, col, radius, y0 = 0) {
  a <- sqrt(3) * (radius / 2)

  # Starting from the y origin,
  # if the column is even, then add the `a` factor
  # In any case, multiple the a factor by twice the number of rows

  y0 + (col %% 2) * a + 2 * row * a
}

.calc_x <- function(row, col, radius, x0 = 0) {
  x0 + col * ((3 * radius) / 2)
}
