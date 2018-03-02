flip_array <- function(x, flip_along) {
  # x_dim <- dim(x)
  # Idea from:
  # https://everydropr.wordpress.com/2012/10/16/a-simple-way-to-flip-a-matrix-or-an-array/
  apply(x, flip_along, rev)
}
