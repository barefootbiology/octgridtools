gamma_correction_array <- function(a, gamma = 1/4) {
  if_else(is.na(as.vector(a)), 0, a) %>%
    (function(x) x^gamma) %>%
    array(dim = dim(a))
}
