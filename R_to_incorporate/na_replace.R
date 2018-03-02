na_replace <- function(x, replacement) {
  if_else(is.na(x), replacement, x)
}
