#' Apply an affine matrix to a coordinate pair
#'
#' Perform an affine transformation on x and y.
#'
#' @export
#' @importFrom tibble as_tibble
affine_transform_coord <- function(df, coords = c("x", "y"), affine) {

  # TASK: This could probably be updated, just incase the coords vector includes
  #       a column named "v1"
  df[, coords] <-
    affine %*% t(as.matrix(df[, coords] %>%
      mutate(v1 = 1))) %>%
    t() %>%
    as.data.frame() %>%
    set_names(c(coords, "v1")) %>%
    select(-v1) %>%
    as_tibble()

  df
}
