#' Draw a group-model claim as one scale-aware segment
#'
#' @noRd
GeomModelMark <- ggplot2::ggproto(
  "GeomModelMark", ggplot2::GeomSegment,
  required_aes = c("x", "y"),
  extra_params = c("na.rm", "width", "mark_axis"),
  setup_data = function(data, params) {
    width <- params$width %||% .4
    if (identical(params$mark_axis, "y")) {
      data$yend <- data$y + width / 2
      data$y <- data$y - width / 2
      data$xend <- data$x
    } else {
      data$xend <- data$x + width / 2
      data$x <- data$x - width / 2
      data$yend <- data$y
    }
    data
  }
)
