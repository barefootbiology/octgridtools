#' Splits a 3D array (x, y, z) along the z axis
#'
#' Splits a 3D array (x, y, z) along the z axis. Returns a list.
#'
#' @export
split_xyz_by_z <- function(a) {
  dim_a <- dim(a)

  lapply(1:dim_a[[3]], function(x)
    a[ , , x])
}
