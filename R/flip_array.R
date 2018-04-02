#' Flip an array along an axis.
#'
#' Flips an array along a specified axis. Idea from
#' https://everydropr.wordpress.com/2012/10/16/a-simple-way-to-flip-a-matrix-or-an-array/
#' NOTE: I'm not sure how well this function behaves.
#'
#' @export
flip_array <- function(x, flip_along) {
  # x_dim <- dim(x)
  # Idea from:
  # https://everydropr.wordpress.com/2012/10/16/a-simple-way-to-flip-a-matrix-or-an-array/
  apply(x, flip_along, rev)
}
