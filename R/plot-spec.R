#' Read the static facts out of a ggplot object
#'
#' Everything downstream consumes the plain list this returns, so a change to
#' ggplot2's object shape breaks one function with direct tests rather than
#' every caller.
#'
#' @param p A ggplot object.
#'
#' @return A list with `mapping`, `data`, `variables`, `aesthetics`, `facets`,
#'   `axes` and `resolve_aes` (looks up an aesthetic's quosure and source
#'   data). Which axis carries a model's outcome is not here -- that belongs
#'   to the plan.
#'
#' @noRd
plot_spec <- function(p) {
  layer <- if (length(p$layers) > 0) p$layers[[1]] else NULL

  # ggformula always maps at plot level, but a plot built with ggplot2 directly may
  # carry its aesthetics on the layer; they are the plot's variables either way
  mapping <- p$mapping
  for (aes in setdiff(names(layer$mapping), names(mapping))) {
    mapping[[aes]] <- layer$mapping[[aes]]
  }

  data <- p$data
  if (!is.data.frame(data) && is.data.frame(layer$data)) data <- layer$data

  aes_names <- sort(setdiff(names(mapping), c("x", "y")))
  facets <- p$facet$vars()
  variables <- sort(c(purrr::map_chr(mapping, as_label), facet = facets))
  axes <- variables[names(variables) %in% aes_names == FALSE & variables %in% facets == FALSE]

  resolve_aes <- function(aes) {
    if (!is.null(p$mapping[[aes]])) {
      return(list(quo = p$mapping[[aes]], data = p$data))
    }
    if (!is.null(layer$mapping[[aes]])) {
      layer_data <- if (is.data.frame(layer$data)) layer$data else p$data
      return(list(quo = layer$mapping[[aes]], data = layer_data))
    }
    NULL
  }

  list(
    mapping = mapping,
    data = data,
    variables = variables,
    aesthetics = variables[aes_names],
    facets = facets,
    axes = axes,
    resolve_aes = resolve_aes
  )
}

#' Read the rendered positions and panel ranges out of a built plot
#'
#' Reads the first layer, which is where the observations live in every
#' documented pipeline.
#'
#' @param p A ggplot object.
#'
#' @return A list with `x`, `y`, `x_range` and `y_range`, the `x_limits` and
#'   `y_limits` those ranges expand,
#'   plus the `x_transform` and `y_transform` they are expressed in. A scale
#'   that has no transformation, such as a discrete one, reports `NULL`.
#'
#' @noRd
plot_geometry <- function(p) {
  geometry_from_build(ggplot2::ggplot_build(p))
}

#' Extract `plot_geometry()`'s fields from an already-built plot
#'
#' Split out of `plot_geometry()` so a caller that also needs something else
#' off the same build -- `overlay_spec()` needs the plot's labels -- can read
#' both off one `ggplot_build()` rather than paying for a second one, which
#' re-runs every stat and repeats any warning the build emits.
#'
#' @param built The return value of `ggplot2::ggplot_build()`.
#'
#' @noRd
geometry_from_build <- function(built) {
  panel <- built$layout$panel_params[[1]]

  list(
    x = built$data[[1]]$x,
    y = built$data[[1]]$y,
    x_range = panel$x.range,
    y_range = panel$y.range,
    # the ranges above carry ggplot2's expansion; the limits are what the data
    # and any expand_limits() trained, which is where an overlay's own headroom
    # has to be measured from
    x_limits = built$layout$panel_scales_x[[1]]$get_limits(),
    y_limits = built$layout$panel_scales_y[[1]]$get_limits(),
    x_transform = built$layout$panel_scales_x[[1]]$get_transformation(),
    y_transform = built$layout$panel_scales_y[[1]]$get_transformation()
  )
}
