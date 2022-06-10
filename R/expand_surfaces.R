#' Add pseudo-surfaces to the layer table.
#'
#' This function adds two new surfaces. They represent the inner-most and outer-
#' most edges of the volume. These surfaces are not segmented by the
#' Iowa Reference Algorithms.
#'
#' @export
#' @importFrom dplyr select distinct bind_rows arrange
#' @importFrom magrittr %>%
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
expand_surfaces <- function(seg) {
  # Get every A-scan and B-scan.
  coordinates <-
    seg$layers %>%
    select(bscan_id, ascan_id) %>%
    distinct()

  # Create new psuedo-surfaces.
  new_surfaces <-
    tibble(
      surface_id = range(seg$layers$surface_id) + c(-1, 1),
      value = c(0, seg$info$size_y)
    ) %>%
    split(.$surface_id) %>%
    map_dfr(
      ~coordinates %>%
        mutate(surface_id = .x$surface_id, value = .x$value)
      )

  # Combine the surfaces.
  seg$layers %>%
    bind_rows(new_surfaces) %>%
    arrange(bscan_id, ascan_id, surface_id)
}
