#' Read the static facts out of a ggplot object
#'
#' Everything downstream consumes the plain list this returns, so a change to
#' ggplot2's object shape breaks one function with direct tests rather than
#' every caller.
#'
#' A pin (`R/plot-pin.R`) rewrites a mapped aesthetic's quosure to a fixed
#' column while remembering what it replaced, so a pinned aesthetic's label
#' and its quosure diverge: `labels` reports the reader's own spelling (what
#' a message should say) and `mapping`/`resolve_aes()` report the pinned
#' quosure (what a build should evaluate).
#'
#' @param p A ggplot object.
#'
#' @return A list with `mapping` (the pinned quosures, so anything that
#'   evaluates gets the values the plot draws), `data` (the pinned plot's
#'   data), `labels` (a named character vector, the reader's spelling of
#'   every mapped aesthetic -- a pinned aesthetic's original label where one
#'   is recorded, `as_label()` of the mapped quosure otherwise), `variables`
#'   (`labels` merged with `facets`), `aesthetics` and `axes` (both narrowed
#'   from `variables`), `facets`, `pins`
#'   (the recorded originals, from `plot_pins()`) and `resolve_aes` (returns a
#'   resolved aesthetic descriptor with `quo`, `data`, reader-facing `label`,
#'   `owner` and `layer_index`). Which axis carries a model's outcome is not
#'   here -- that belongs to the plan.
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

  pins <- plot_pins(p)
  labels <- purrr::imap_chr(mapping, function(quo, a) {
    if (!is.null(pins[[a]])) as_label(pins[[a]]) else as_label(quo)
  })

  aes_names <- sort(setdiff(names(mapping), c("x", "y")))
  facets <- p$facet$vars()
  variables <- sort(c(labels, facet = facets))
  axes <- variables[names(variables) %in% aes_names == FALSE & variables %in% facets == FALSE]

  resolve_aes <- function(aes) {
    if (!is.null(p$mapping[[aes]])) {
      source_data <- p$data
      if (!is.data.frame(source_data) && is.data.frame(layer$data)) {
        source_data <- layer$data
      }
      return(list(
        quo = p$mapping[[aes]], data = source_data, label = labels[[aes]],
        owner = "plot", layer_index = NA_integer_
      ))
    }
    if (!is.null(layer$mapping[[aes]])) {
      layer_data <- if (is.data.frame(layer$data)) layer$data else p$data
      return(list(
        quo = layer$mapping[[aes]], data = layer_data, label = labels[[aes]],
        owner = "layer", layer_index = 1L
      ))
    }
    NULL
  }

  list(
    mapping = mapping,
    data = data,
    labels = labels,
    variables = variables,
    aesthetics = variables[aes_names],
    facets = facets,
    axes = axes,
    pins = pins,
    resolve_aes = resolve_aes
  )
}
