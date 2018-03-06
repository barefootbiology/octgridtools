map_volume_xy <- function(volume, seg_volume, .fun, ...) {
  regions <- unique_regions(seg_volume = seg_volume)

  result <- list()

  # TASK: Is it worth replacing this with a `purrr::map` function?
  for(r in regions) {
    # result[[as.character(r)]] <- .fun(volume[seg_volume == r], ...)

    slice_r <- volume * create_slicer_subset(seg_volume = seg_volume, values = r)

    result[[as.character(r)]] <- .fun(slice_r, ...)
  }

  result
}
