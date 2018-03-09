#' Summarize layer thickness across defined regions
#'
#' Summarize segmented retinal layer thickness across regions defined by a grid.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter group_by summarize ungroup full_join mutate
summarize_cell_thickness <- function(pts_cell, cell_centers) {
  # TASK: Update this function to all for a list of functions to be used to
  #       compute the per cell statistics

  pts_cell %>%
    select(-x, -y) %>%
    filter(!is.na(.cell_id)) %>%
    group_by(surface_id, .cell_id) %>%
    dplyr::summarize(
      thickness_um_mean = mean(thickness_um),
      thickness_um_sd = sd(thickness_um)
    ) %>%
    ungroup() %>%
    full_join(cell_centers) %>%
    dplyr::mutate(thickness_um_mean_label = sprintf(
      "%.2f",
      thickness_um_mean
    ))
}
