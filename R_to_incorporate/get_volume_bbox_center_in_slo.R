get_volume_bbox_center_in_slo <- function(volume) {
  get_volume_bbox_in_slo(volume) %>%
    mutate(x = mean(c(xmin, xmax)),
           y = mean(c(ymin, ymax))) %>%
    select(x, y)
}