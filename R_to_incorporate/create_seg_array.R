# This function works with IRA/OCTExplorer segmenation coordinate definition.
create_seg_array <- function(segmentation) {
  values_ordered <- segmentation$layers %>%
    arrange(layer_y_order, bscan_id, ascan_id) %>%
    select(value) %>%
    as_vector()
  
  array(
    c(
      rep(0, segmentation$info$size_x * segmentation$info$size_z),
      values_ordered,
      rep(segmentation$info$size_y, segmentation$info$size_x * segmentation$info$size_z)
      ),
    dim = c(segmentation$info$size_x, 
            segmentation$info$size_z, 
            segmentation$info$surface_num + 2)
    )
}