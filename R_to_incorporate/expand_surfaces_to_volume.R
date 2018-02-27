expand_surfaces_to_volume <- function(surface_array, vol_dim) {
  n_voxels <- 
    vol_dim %>% 
    cumprod() %>% 
    max()
  
  n_surfaces <- dim(surface_array)[3]
  
  surface_array_padded <- create_seg_array(example$segmentation)
  
  result_vol <- array(rep(NA, n_voxels), dim = vol_dim)
  
  layer_thicknesses <- 
    surface_array_padded[ , , c(2:13)] - 
    surface_array_padded[ , , c(1:12)] 
  
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