#' Generate hexagon centers
#'
#' Generate the center points associated with a grid of hexagonal cells.
#'
#' @export
#' @importFrom dplyr rowwise mutate ungroup
#' @importFrom magrittr %>%
generate_hex_centers <- function(rows, columns, radius, x0=0, y0=0) {
  expand.grid(1:rows, 1:columns) %>%
    setNames(c("row", "column")) %>%
    rowwise() %>%
    mutate(
      center_x = .calc_x(i = row, j = column, radius, x0 = x0),
      center_y = .calc_y(i = row, j = column, radius, y0 = y0)
    ) %>%
    ungroup()
}
