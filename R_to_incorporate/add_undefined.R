# NOTE: This function assumes there is a column named "value", but this column
#       name is likely to change in future interations of the software.
add_undefined <- function(data, undefined) {
  data %>%
    full_join(undefined) %>%
    mutate(is_defined = if_else(is.na(is_defined), TRUE, is_defined)) %>%
    mutate(value_defined = if_else(is_defined, value, as.numeric(NA)))
}
