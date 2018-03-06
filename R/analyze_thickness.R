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
                              layer_definition,
                              return_objects = FALSE,
                              flip_region = "OD") {

  # IRA segmentation surfaces
  segmentation <- read_segmentation_xml(segmentation_file)

  # Manually annotated center of the fovea
  grid_center <- read_center_xml(grid_center_file)


  # PARSE COORDINATES ----------------------------------------------------------
  center_x_voxel <- grid_center[["center"]][["x"]]
  center_z_voxel <- grid_center[["center"]][["z"]]

  center_x <- center_x_voxel * segmentation$info$voxel_size_x
  center_z <- center_z_voxel * segmentation$info$voxel_size_z

  # TASK: QUESTION: Does this matrix only apply to the ETDRS grid or to any
  #       grid?
  # NOTE: This approach will fail for downsampled VOL files, as the laterality
  #       is lost!
  # TASK: Rework this so that I don't have to perform this flip here.
  laterality <- grid_center[["scan_characteristics"]][["laterality"]][[1]]

  flip_y <- if (flip_region == laterality) 1 else -1

  affine_matrix <- matrix(
    c(
      flip_y, 0, center_x,
      0, -1, center_z,
      0, 0, 1
    ),
    byrow = TRUE, ncol = 3
  )

  # Using affine transformation, flip the coordinates about the origin on
  # the vertical axis, then translate the coordinates to the foveal
  # coordinates (center_x, center_y).
  grid_regions_segments <-
    grid_regions %>%
    affine_transform_coord(c("x", "y"), affine = affine_matrix)

  reg_centers <-
    grid_regions_segments %>%
    group_by(.cell_id) %>%
    dplyr::summarize(
      center_x = mean(x),
      center_z = mean(y)
    ) %>%
    ungroup()

  regions_polygons <- region_segments_to_spatialpolygons(grid_regions_segments)

  segmentation_thickness <- compute_layer_thickness(segmentation)

  segmentation_thickness_points <- tibble_to_spatialpoints(segmentation_thickness)

  points_in_regions <- find_points_in_regions(
    segmentation_thickness_points,
    regions_polygons,
    segmentation_thickness
  )

  layer_region_thickness_summarized <- summarize_region_thickness(
    points_in_regions,
    reg_centers
  )

  if (!return_objects) {
    return(layer_region_thickness_summarized)
  } else {
    return(list(
      thickness = layer_region_thickness_summarized,
      segmentation = segmentation,
      center = grid_center
    ))
  }
}
