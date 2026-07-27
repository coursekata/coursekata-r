#' Name a layer we add, so it can be found without counting
#'
#' @param layer A ggplot2 layer.
#' @param tag A single string naming it.
#'
#' @return The layer, tagged.
#'
#' @noRd
tag_layer <- function(layer, tag) {
  attr(layer, "coursekata_layer") <- tag
  layer
}

#' Find a tagged layer's position in a plot
#'
#' @param p A ggplot object.
#' @param tag The tag to look for.
#'
#' @return The integer index into `p$layers`, or `NA_integer_` if absent.
#'
#' @noRd
layer_index <- function(p, tag) {
  hits <- which(vapply(
    p$layers,
    function(l) identical(attr(l, "coursekata_layer"), tag),
    logical(1)
  ))
  if (length(hits) == 0) NA_integer_ else as.integer(hits[[1]])
}
