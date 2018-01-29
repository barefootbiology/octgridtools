#' Apply an affine matrix to a coordinate pair
#'
#' Perform an affine transformation on x and y.
#'
#' @export
affine_transform_coord <- function(x, y, affine) {
  result <- affine %*% c(x, y, 1)

  return(data.frame(x = result[1,],
                    y = result[2,]))
}
