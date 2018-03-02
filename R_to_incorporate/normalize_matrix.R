normalize_matrix <- function(m) {
  dim_m <- dim(m)

  m %>%
    as.vector() %>%
    (function(i) (i - min(i, na.rm = TRUE)) / max(i - min(i, na.rm = TRUE),
                                                  na.rm = TRUE)) %>%
    array(dim = dim_m)
}
