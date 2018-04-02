#' Overlay a B-scan with another image of the same dimensions
#'
#' Overlays an B-scan image with another image, such as segmentation surfaces,
#' which is the same dimensions as the original B-scan.
#' This function facilitates combining two rasters with different fill
#' aesthetics. NOTE: I'm not sure if this will work with unregistered VOL files.
#'
#' @export
#' @importFrom ggplot2 ggsave annotation_raster
#' @importFrom magick image_read
annotation_overlay_bscan <- function(plot_overlay, volume) {

  temp_png <- tempfile()

  dpi <- 72

  ggsave(plot_overlay,
         filename = temp_png,
         device = "png",
         units = "in",
         width = volume$header$size_x * 2 / dpi,
         height = volume$header$size_z * 2 / dpi,
         bg = "transparent")

  overlay <- image_read(temp_png)

  unlink(temp_png)

  annotation_raster(raster = overlay,
                    xmin = 1,
                    xmax = volume$header$size_x,
                    ymin = -1,
                    ymax = -1 * volume$header$size_z,
                    interpolate = FALSE)
}
