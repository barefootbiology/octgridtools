#' Compute layer thickness in voxels
#'
#' Compute per bscan layer thickness for every ascan.
#'
#' @export
#' @importFrom dplyr group_by arrange mutate filter ungroup select
#' @importFrom magrittr %>%
compute_layer_thickness <- function(seg) {
  seg$layers %>%
    group_by(bscan_id, ascan_id) %>%
    arrange(bscan_id, ascan_id) %>%
    mutate(thickness_voxels = lead(value) - value) %>%
    filter(!is.na(thickness_voxels)) %>%
    # Convert thickness to millimeters
    mutate(thickness_um = thickness_voxels *
             seg$info$voxel_size_y * 1000) %>%
    ungroup() %>%
    mutate(x = ascan_id * seg$info$voxel_size_x,
           y = bscan_id * seg$info$voxel_size_z) %>%
    select(layer_y_order, ascan_id, x, bscan_id, y, thickness_um) %>%
    return()
}