# TASK: Consider renaming this function "create_slicer_range"
create_slicer_range <- function(seg_volume, from, to) {
  slicer <- (seg_volume >= from) & (seg_volume <= to) * 1

  slicer[slicer == 0] <- NA

  slicer
}

create_slicer_subset <- function(seg_volume, values) {
  slicer <- array(seg_volume %in% values, dim = dim(seg_volume)) * 1

  slicer[slicer == 0] <- NA

  slicer
}
