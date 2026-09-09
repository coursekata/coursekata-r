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

#' Find every layer in a plot carrying a tag
#'
#' Sets of layers this package adds can share one tag on purpose. Repeated
#' helper calls may add another complete set, and `layer_index()`
#' answers with the first hit, which is what its existing callers rely on.
#'
#' @param p A ggplot object.
#' @param tag The tag to look for.
#'
#' @return An integer vector of every matching index, in plot order; empty when
#'   nothing carries the tag.
#'
#' @noRd
layer_indices <- function(p, tag) {
  as.integer(which(vapply(
    p$layers,
    function(l) identical(attr(l, "coursekata_layer"), tag),
    logical(1)
  )))
}

#' Replace every layer carrying one package tag
#'
#' ggplot2 4 plots are S7 objects whose layer collection must be replaced
#' through the property interface. Keep that write in one helper so callers do
#' not depend on the container representation.
#'
#' @param p A ggplot object.
#' @param tag The tag to replace.
#' @param replacements Replacement layers, inserted where the first match was.
#'
#' @return A copied ggplot object.
#' @noRd
replace_tagged_layers <- function(p, tag, replacements = list()) {
  indices <- layer_indices(p, tag)
  if (length(indices) == 0L) {
    insertion <- length(p$layers) + 1L
    kept <- p$layers
  } else {
    insertion <- indices[[1L]]
    kept <- p$layers[-indices]
  }
  insertion <- min(insertion, length(kept) + 1L)
  before <- if (insertion > 1L) kept[seq_len(insertion - 1L)] else list()
  after <- if (insertion <= length(kept)) kept[insertion:length(kept)] else list()
  p@layers <- c(before, replacements, after)
  p
}
