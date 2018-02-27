create_slicer <- function(seg_volume, from, to) {
  slicer <- (seg_volume >= from) & (seg_volume <= to) * 1
  
  slicer[slicer == 0] <- NA
  
  slicer
}