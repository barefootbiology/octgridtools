# Compute volume statistics on regions within a volume
unique_regions <- function(seg_volume) {
  seg_volume %>%
    as.vector() %>%
    unique() %>%
    sort()
}