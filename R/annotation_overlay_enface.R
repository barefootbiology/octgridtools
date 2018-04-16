#' Overlay an en face image with another en face image
#'
#' Overlays an en face image with another en face image computed from the volume
#' data.This function facilitates combining two rasters with different fill
#' aesthetics. NOTE: I'm not sure if this will work with unregistered VOL files.
#'
#' @export
#' @importFrom ggplot2 ggsave annotation_raster
#' @importFrom magick image_read
annotation_overlay_enface <- function(plot_overlay, volume,
                                      xmin, xmax, ymin, ymax) {

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

  annotation_raster(raster = overlay,
                    xmin = xmin,
                    xmax = xmax,
                    ymin = ymin,
                    ymax = ymax,
                    interpolate = FALSE)
}
