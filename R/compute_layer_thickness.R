#' Compute layer thickness in voxels
#'
#' Compute per bscan layer thickness for every ascan.
#'
#' @export
#' @importFrom dplyr group_by arrange mutate filter ungroup select
#' @importFrom magrittr %>%
compute_layer_thickness <- function(seg, expand_surfaces = FALSE) {
  layers <- seg$layers

  if(expand_surfaces) {
    layers <- expand_surfaces(seg)
  }

  layers %>%
    group_by(bscan_id, ascan_id) %>%
    arrange(bscan_id, ascan_id) %>%
    mutate(thickness_voxel = lead(value) - value) %>%
    filter(!is.na(thickness_voxel)) %>%
    # Convert thickness to microns
    mutate(
      thickness_um = thickness_voxel * seg$info$voxel_size_y * 1000
      ) %>%
    ungroup() %>%
    mutate(
      x = ascan_id * seg$info$voxel_size_x,
      y = bscan_id * seg$info$voxel_size_z
    ) %>%
    # NOTE: The output is now being referenced to the x, y of the
    # grid, not the x, z of the volume.
    select(surface_id, ascan_id, x, bscan_id, y, thickness_voxel, thickness_um)
}
