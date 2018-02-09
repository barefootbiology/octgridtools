#' Generate hexagon centers
#'
#' Generate the center points associated with a grid of hexagonal cells.
#'
#' @export
#' @importFrom dplyr rowwise mutate ungroup
#' @importFrom magrittr %>%
generate_hexagon_centers <- function(nrow, ncol, radius, x0=0, y0=0) {
  expand.grid(1:nrow, 1:ncol) %>%
    setNames(c("row_id", "col_id")) %>%
    rowwise() %>%
    mutate(
      center_x = .calc_x(i = row_id, j = col_id, radius, x0 = x0),
      center_y = .calc_y(i = row_id, j = col_id, radius, y0 = y0)
    ) %>%
    ungroup() %>%
    mutate(.cell_id = 1:n())
}
