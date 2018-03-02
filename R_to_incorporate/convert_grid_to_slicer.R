# Take a grid (x, y)
# Return a slicer volume (x, y, z)
# Center point in voxel space with scaling to anatomic space

# TASK: Write a function to create a slicer for a given volume by supplying a
#       volume, a grid (flipped for OS if necessary), a center point, and an
#       appropriate scaling values.
#       Consider that most grids are probably specified for SLO/anatomic
#       scales, so we might need to flip the grid along the horizontal axis
#       before applying it to the volume.

# Basic procedure:
# 1. From the volume, get the 2D enface coordinates (x, y).
# 2. Translate these coordinates into anatomic space.
# 3. Intersect these coordinates with the grid cells in anatomic space.
# 4. Map these intersections back to the original x, y coordinates of the
#    volume.
# 5. Construct a volume with appropriate cell ID's as values in each voxel.

convert_grid_to_slicer <- function(grid, grid_center, dims, scale_x, scale_y) {

  volume_points <- expand.grid(vol_x = 1:dims[[1]],
                               vol_y = 1:dims[[2]]) %>%
    as_tibble() %>%
    mutate(vol_x_centered = vol_x - grid_center$center$x[[1]],
           vol_y_centered = vol_y - grid_center$center$z[[1]]) %>%
    mutate(x = vol_x_centered * scale_x,
           y = vol_y_centered * scale_y)

  # Get 2D (ascan, bscan) coordinates.
  # Center on 0 using the grid_center
  # Translate to anatomic scale
  # For all the points, intersect with grid_cells.
  # Bind the intersection information.
  # Combine with original space

  # Construct an result array the same dimensions as the original volume

  grid_transformed <- grid

  cells_sppolygons <- cells_to_spatialpolygons(grid_transformed)

  volume_sppoints <- tibble_to_spatialpoints(volume_points)

  voxels_in_cells <- octgridtools:::find_sppoints_in_sppolygons(
    volume_sppoints,
    cells_sppolygons,
    volume_points %>% select(-x, -y)
  )

  # result <- array(rep(as.integer(NA), max(cumprod(dims))), dim = dims)

  # cell_ids <- points_in_cells %>%
  #   filter(!is.na(.cell_id)) %>%
  #   pluck(".cell_id") %>%
  #   unique()

  cell_matrix <- voxels_in_cells %>%
    select(vol_x, vol_y, .cell_id) %>%
    reshape2::acast(vol_x ~ vol_y)

  array(cell_matrix, dim = dims)
}
