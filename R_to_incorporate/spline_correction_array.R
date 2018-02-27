spline_correction_array <- function(a) {
  if_else(is.na(as.vector(a)), 0, a) %>%
    heyexr::spline_correction() %>%
    array(dim = dim(a)) 
}