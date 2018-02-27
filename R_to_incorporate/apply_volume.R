apply_volume <- function(volume, seg_volume, .fun, ...) {
  regions <- unique_regions(seg_volume = seg_volume)
  
  # TASK: See if you can replace this loop with a purrr::walk
  for(r in regions) {
    volume[seg_volume == r] <- .fun(as.vector(volume[seg_volume == r]),
                                    ...)
  }
  
  volume
}