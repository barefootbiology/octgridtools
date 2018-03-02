split_xyz_by_z <- function(a) {
  dim_a <- dim(a)

  lapply(1:dim_a[[3]], function(x)
    a[ , , x])
}
