find_replace <- function(.x, replacement, find_fun, ...) {
  if_else(find_fun(.x, ...), replacement, x)
}

na_replace <- function(x, replacement) {
  find_replace(.x = x, replacement = replacement, find_fun = is.na)
  # if_else(is.na(x), replacement, x)
}
