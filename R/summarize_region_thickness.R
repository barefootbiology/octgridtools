#' Summarize layer thickness across defined regions
#'
#' Summarize segmented retinal layer thickness across regions defined by a grid.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter group_by summarize ungroup full_join mutate
summarize_region_thickness <- function(pts_reg, reg_centers) {
  pts_reg %>%
    select(-x, -y) %>%
    filter(!is.na(sector_id)) %>%
    group_by(layer_y_order, sector_id) %>%
    dplyr::summarize(thickness_um_mean = mean(thickness_um),
                     thickness_um_sd = sd(thickness_um)) %>%
    ungroup() %>%
    full_join(reg_centers) %>%
    dplyr::mutate(thickness_um_mean_label = sprintf("%.2f",
                                                    thickness_um_mean))
}
