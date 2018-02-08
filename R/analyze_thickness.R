#' Compute thickness across OCT segmentations using a grid.
#'
#' Compute regional thickness for segmentated OCT data.
#'
#' @export
#' @importFrom heyexr read_segmentation_xml read_center_xml
#' @importFrom dplyr select rename group_by summarize ungroup
#' @importFrom magrittr %>%
#' @importFrom purrrlyr by_row
analyze_thickness <- function(segmentation_file,
                              grid_center_file,
                              grid_regions,
                              layer_definition) {

  # IRA segmentation surfaces
  segmentation <- read_segmentation_xml(segmentation_file)

  # Manually annotated center of the fovea
  grid_center <- read_center_xml(grid_center_file)


  # PARSE COORDINATES ----------------------------------------------------------
  # Add one to use the 1-based coordinate convention in R.
  center_x_voxel <- grid_center[["center"]][["x"]][[1]] + 1
  center_z_voxel <- grid_center[["center"]][["z"]][[1]] + 1

  center_x <- center_x_voxel * segmentation$info$voxel_size_x
  center_z <- center_z_voxel * segmentation$info$voxel_size_z


  affine_matrix <- matrix(c(1, 0, center_x,
                            0, -1, center_z,
                            0, 0, 1),
                          byrow = TRUE, ncol = 3)

  # Create the grid in millimeter coordinates.
  # Using affine transformation, flip the coordinates about the origin on
  # the vertical axis, then translate the coordinates to the foveal
  # coordinates (center_x, center_y).
  grid_regions_segments <- make_grid_sectors(grid_regions,
                                             center_x = 0,
                                             center_y = 0) %>%
    by_row(~affine_transform_coord(x = .$x,
                                             y = .$y,
                                             affine = affine_matrix),
                     .collate = "cols") %>%
    select(-x, -y) %>%
    rename(x = x1, y = y1)

  reg_centers <- grid_regions_segments %>%
    group_by(sector_id) %>%
    dplyr::summarize(center_x = mean(x),
                     center_z = mean(y)) %>%
    ungroup()

  regions_polygons <- region_segments_to_spatialpolygons(grid_regions_segments)

  segmentation_thickness <- compute_layer_thickness(segmentation)

  segmentation_thickness_points <- tibble_to_spatialpoints(segmentation_thickness)

  points_in_regions <- find_points_in_regions(segmentation_thickness_points,
                                              regions_polygons,
                                              segmentation_thickness)

  layer_region_thickness_summarized <- summarize_region_thickness(points_in_regions,
                                                                  reg_centers)

  return(layer_region_thickness_summarized)

}
