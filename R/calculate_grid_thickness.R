#' Compute thickness across OCT segmentations using a grid.
#'
#' Compute regional thickness for segmentated OCT data.
#'
#' @export
#' @importFrom heyexr read_segmentation_xml read_center_xml
#' @importFrom dplyr select rename group_by summarize ungroup
#' @importFrom magrittr %>%
#' @importFrom purrrlyr by_row
calculate_grid_thickness <- function(segmentation,
                                     grid_center,
                                     grid,
                                     flip_region = "OD",
                                     laterality = grid_center[["scan_characteristics"]][["laterality"]][[1]]) {

  # TASK: Add more error checking here.
  if(!(toupper(laterality) %in% c("OD", "OS"))) {
    error('laterality must be either "OD" or "OS". Is laterality specified in the grid_center file?')
  }

  # TASK: Should this throw an error or simply a warning?
  if(!(toupper(flip_region) %in% c("OD", "OS"))) {
    error('flip_region must be either "OD" or "OS".')
  }


  # PARSE COORDINATES ----------------------------------------------------------
  # Add one to use the 1-based coordinate convention in R.
  center_x_voxel <- grid_center[["center"]][["x"]]
  center_z_voxel <- grid_center[["center"]][["z"]]

  center_x <- center_x_voxel * segmentation$info$voxel_size_x
  center_z <- center_z_voxel * segmentation$info$voxel_size_z

  # TASK: QUESTION: Does this matrix only apply to the ETDRS grid or to any
  #       grid?
  # NOTE: This approach will fail for downsampled VOL files, as the laterality
  #       is lost!
  # TASK: Rework this so that I don't have to perform this flip here.
  # laterality <- grid_center[["scan_characteristics"]][["laterality"]][[1]]

  flip_y <- if (flip_region == laterality) 1 else -1

  affine_matrix <- matrix(
    c(
      flip_y, 0, center_x,
      0, 1, center_z,
      0, 0, 1
    ),
    byrow = TRUE, ncol = 3
  )

  # TASK: Check to see if the vertical flip is necessary.

  # Using affine transformation, flip the coordinates about the origin on
  # the vertical axis, then translate the coordinates to the foveal
  # coordinates (center_x, center_y).
  grid_transformed <-
    grid %>%
    affine_transform_coord(c("x", "y"), affine = affine_matrix)

  cell_centers <-
    grid_transformed %>%
    group_by(.cell_id) %>%
    dplyr::summarize(
      center_x = mean(x),
      center_z = mean(y)
    ) %>%
    ungroup()

  cells_sppolygons <- cells_to_spatialpolygons(grid_transformed)

  segmentation_thickness <- compute_layer_thickness(segmentation)

  segmentation_thickness_points <- tibble_to_spatialpoints(segmentation_thickness)

  points_in_cells <- find_sppoints_in_sppolygons(
    segmentation_thickness_points,
    cells_sppolygons,
    segmentation_thickness
  )

  summarize_cell_thickness(points_in_cells, cell_centers)

}
