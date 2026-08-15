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
  mapping <- p$mapping
  aes_names <- sort(setdiff(names(mapping), c("x", "y")))
  facets <- p$facet$vars()
  variables <- sort(c(purrr::map_chr(mapping, as_label), facet = facets))
  axes <- variables[names(variables) %in% aes_names == FALSE & variables %in% facets == FALSE]

  resolve_aes <- function(aes) {
    if (!is.null(mapping[[aes]])) {
      return(list(quo = mapping[[aes]], data = p$data))
    }
    if (length(p$layers) > 0) {
      layer <- p$layers[[1]]
      if (!is.null(layer$mapping[[aes]])) {
        layer_data <- if (is.data.frame(layer$data)) layer$data else p$data
        return(list(quo = layer$mapping[[aes]], data = layer_data))
      }
    }
    NULL
  }

  list(
    mapping = mapping,
    data = p$data,
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
#' @return A list with `x`, `y`, `x_range` and `y_range`, plus the
#'   `x_transform` and `y_transform` those ranges are expressed in. A scale
#'   that has no transformation, such as a discrete one, reports `NULL`.
#'
#' @noRd
plot_geometry <- function(p) {
  built <- ggplot2::ggplot_build(p)
  panel <- built$layout$panel_params[[1]]

  list(
    x = built$data[[1]]$x,
    y = built$data[[1]]$y,
    x_range = panel$x.range,
    y_range = panel$y.range,
    x_transform = built$layout$panel_scales_x[[1]]$get_transformation(),
    y_transform = built$layout$panel_scales_y[[1]]$get_transformation()
  )
}
