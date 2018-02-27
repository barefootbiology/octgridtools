annotation_overlay <- function(plot_overlay, volume) {
  
  temp_png <- tempfile()
  
  dpi <- 72
  
  ggsave(plot_overlay, 
         filename = temp_png, 
         device = "png", 
         units = "in",
         width = volume$header$size_x * 2 / dpi,
         height = volume$header$num_bscans * 2 / dpi, 
         bg = "transparent")
  
  overlay <- magick::image_read(temp_png)
  
  unlink(temp_png)
  
  angle <- get_volume_angle_in_slo(volume)
  
  overlay_rotated <- magick::image_rotate(overlay,
                                          degrees = degree_to_radian(angle))
  
  overlay_bbox <- get_volume_bbox_in_slo(volume) 
  
  annotation_raster(raster = overlay_rotated, 
                    xmin = overlay_bbox$xmin,
                    xmax = overlay_bbox$xmax,
                    ymin = -1 * overlay_bbox$ymin,
                    ymax = -1 * overlay_bbox$ymax, 
                    interpolate = FALSE)
}