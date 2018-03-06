melt_array <- function(x, nm) {
  reshape2::melt(data = x) %>%
    set_names(nm = nm) %>%
    as_tibble()
}
