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
#' Sets of layers this package adds share one tag on purpose -- a second
#' `show_cutoffs()` call adds a second complete set -- and `layer_index()`
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
