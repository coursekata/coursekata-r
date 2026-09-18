# Coefficient coordinates are stat output, not constructor-time layer data.
coefficient_test_mark <- function(plot, tag) {
  index <- layer_index(plot, tag)
  layer <- plot$layers[[index]]
  data <- ggplot2::ggplot_build(plot)$data[[index]]
  params <- layer$geom_params
  if (nrow(data) && ".b_x_just" %in% names(data)) {
    params$x_just <- data$.b_x_just[[1L]]
    params$y_just <- data$.b_y_just[[1L]]
  }
  columns <- intersect(c("x", "y", "xend", "yend", "label"), names(data))
  appearance <- intersect(c("colour", "size", "linewidth", "alpha", "shape", "fill", "stroke"), names(data))
  aesthetics <- lapply(data[appearance], function(values) values[[1L]])
  layer_with(layer, data = data[columns], geom_params = params, aes_params = aesthetics)
}
