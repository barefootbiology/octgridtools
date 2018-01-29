#' Generate a grid of rectangles
#'
#' Generates a grid of rectangles around a center point
#'
#' @export
#' @importFrom dplyr mutate
#' @importFrom scales rescale
#' @importFrom magrittr %>%
make_grid_rectangles <- function(center_x, center_y, nrow, ncol, width) {
  result <- data.frame(region_id = 1:(nrow*ncol),
                       row_id = rep(1:nrow, each = ncol),
                       col_id = rep(1:ncol, times = nrow)) %>%
    mutate(x = col_id * width,
           y = row_id * width) %>%
    mutate(x = rescale(x = x,
                       to = c((center_x - (width * (ncol - 1)) / 2),
                              (center_x + (width * (ncol - 1)) / 2)))) %>%
    mutate(y = rescale(x = y,
                       to = c((center_y - (width * (ncol - 1)) / 2),
                              (center_y + (width * (ncol - 1)) / 2)))) %>%
    mutate(xmin = x - (width / 2),
           ymin = y - (width / 2),
           xmax = x + (width / 2),
           ymax = y + (width / 2))

  return(result)
}
