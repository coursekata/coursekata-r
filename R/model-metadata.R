#' Keep the supplied fitted claim available to model annotations
#'
#' A prepared prediction grid cannot recover treatment coding or a weighted
#' fit's original coefficients. Retain the fitted object by reference (R copies
#' it only on modification), including when the layer itself is copied.
#' @noRd
model_layer_fit <- function(layer) attr(layer, "coursekata_model_fit")

#' @noRd
with_model_layer_fit <- function(layer, model) {
  if (inherits(model, "lm")) attr(layer, "coursekata_model_fit") <- model
  layer
}
