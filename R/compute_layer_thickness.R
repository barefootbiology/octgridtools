#' Compute layer thickness in voxels
#'
#' Compute per bscan layer thickness for every ascan.
#'
#' @export
#' @importFrom dplyr group_by arrange mutate filter ungroup select
#' @importFrom magrittr %>%
compute_layer_thickness <- function(seg) {
  seg$layers %>%
    # ------------
    # Remove ascans which were marked by the Iowa Reference Algorithms as
    # unreliable.
    anti_join(seg$undefined_region) %>%
    # ------------
    group_by(bscan_id, ascan_id) %>%
    arrange(bscan_id, ascan_id) %>%
    mutate(thickness_voxels = lead(value) - value) %>%
    filter(!is.na(thickness_voxels)) %>%
    # Convert thickness to millimeters
    mutate(thickness_um = thickness_voxels *
      seg$info$voxel_size_y * 1000) %>%
    ungroup() %>%
    mutate(
      x = ascan_id * seg$info$voxel_size_x,
      y = bscan_id * seg$info$voxel_size_z
    ) %>%
    # NOTE: The output is now being referenced to the x, y of the
    # grid, not the x, z of the volume.
    select(surface_id, ascan_id, x, bscan_id, y, thickness_um)
}
