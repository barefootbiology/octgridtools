#' Expand segmentation surfaces into a segmentation volume
#'
#' Expands segmentation surfaces into a segmentation volume. Surfaces IDs are
#' imputed from the inner inner surface of a layer. NOTE: This assumes surfaces
#' IDs start at 1. Creates artificial surfaces at the top and bottoms of the
#' volume.
#'
#' @export
#' @importFrom magrittr %>%
expand_surfaces_to_volume <- function(surface_array, vol_dim) {
  n_voxels <-
    vol_dim %>%
    cumprod() %>%
    max()

  n_surfaces <- dim(surface_array)[3]

  surface_array_padded <-
    abind::abind(
      array(rep(0, vol_dim[1] * vol_dim[2]), dim = vol_dim[1:2]),
      surface_array,
      array(rep(vol_dim[3], vol_dim[1] * vol_dim[2]), dim = vol_dim[1:2]),
      along = 3
    )

  result_vol <- array(rep(NA, n_voxels), dim = vol_dim)

  layer_thicknesses <-
    surface_array_padded[ , , c(2:(n_surfaces + 2))] -
    surface_array_padded[ , , c(1:(n_surfaces + 1))]

  for(i in 1:dim(result_vol)[1]) {
    for(j in 1:dim(result_vol)[2]) {

      lengths <- layer_thicknesses[i, j, ]
      values <- 0:n_surfaces

      result_vol[i, j, ] <-
        inverse.rle(
          list(
            lengths = layer_thicknesses[i, j, ],
            values = 0:n_surfaces
          )
        )
    }
  }

  result_vol
}
