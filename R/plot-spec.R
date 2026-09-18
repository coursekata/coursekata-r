#' Select the observation layer a plot annotation describes
#'
#' Points take precedence over summaries such as boxplots. Within a role the
#' first layer wins. Package annotations are never a new source of observations.
#' Keeping this decision separate from resolution also lets jitter use the same
#' layer without evaluating a data callback.
#' @noRd
plot_source_index <- function(p) {
  candidates <- which(vapply(p$layers, function(layer) {
    is.null(attr(layer, "coursekata_layer"))
  }, logical(1)))
  points <- candidates[vapply(p$layers[candidates], function(layer) {
    inherits(layer$geom, "GeomPoint") && inherits(layer$stat, "StatIdentity")
  }, logical(1))]
  if (length(points)) points[[1]] else if (length(candidates)) candidates[[1]] else NA_integer_
}

#' Resolve a layer's effective mapping using ggplot2 inheritance rules
#' @noRd
source_mapping <- function(mapping, inherited, inherit.aes = TRUE, drop.null = TRUE) {
  result <- if (isTRUE(inherit.aes)) inherited else ggplot2::aes()
  result <- result %||% ggplot2::aes()
  result[names(mapping)] <- mapping
  if (drop.null) result[!vapply(result, is.null, logical(1))] else result
}

#' Retain the user's expression when a live mapping has a reproducible seed
#' @noRd
source_mapping_label <- function(quo) {
  as_label(attr(quo, "coursekata_original_mapping") %||% quo)
}

#' Restore a derived label only while its prepared mapping still supplies it
#' @noRd
source_label_restore <- function(prepared, original, previous = NULL) {
  force(prepared)
  force(original)
  force(previous)
  function(label) {
    if (identical(label, prepared)) label <- original
    if (is.function(previous)) previous(label) else label
  }
}

#' Read the source rows and mappings used by the selected observation layer
#'
#' Data ownership is independent of mapping ownership: an inherited aesthetic
#' still evaluates on a layer's own rows. Layer$layer_data() is ggplot2's data
#' resolver, including its function, formula (fortified at construction), NULL,
#' and waiver handling. This reader does not build, transform, or drop rows.
#' @noRd
plot_source <- function(p, resolve.data = TRUE) {
  index <- plot_source_index(p)
  layer <- if (!is.na(index)) p$layers[[index]] else NULL
  mapping <- if (is.null(layer)) p$mapping else {
    source_mapping(layer$mapping, p$mapping, layer$inherit.aes)
  }
  data <- if (is.null(layer)) p$data else if (!resolve.data) {
    # Read the stored binding directly. `$data` wraps callbacks as a
    # ggproto_method, which is not a valid ggplot2 layer data function.
    get("data", envir = layer)
  } else {
    with_random_seed_restored(layer$layer_data(p$data))
  }
  list(mapping = mapping, data = data, layer = layer, layer_index = index)
}

#' Read a plot through its observation source, retaining original pin labels
#' @noRd
plot_spec <- function(p) {
  source <- plot_source(p)
  mapping <- source$mapping
  data <- source$data
  pins <- plot_pins(p)
  labels <- purrr::imap_chr(mapping, function(quo, a) {
    if (!is.null(pins[[a]])) source_mapping_label(pins[[a]]) else source_mapping_label(quo)
  })
  aes_names <- sort(setdiff(names(mapping), c("x", "y")))
  facets <- p$facet$vars()
  variables <- sort(c(labels, facet = facets))
  axes <- variables[names(variables) %in% c("x", "y")]

  resolve_aes <- function(aes) {
    quo <- mapping[[aes]]
    if (is.null(quo)) return(NULL)
    local <- source$layer$mapping[[aes]]
    plot_owned <- is.null(source$layer) ||
      (isTRUE(source$layer$inherit.aes) &&
        (is.null(local) || identical(local, p$mapping[[aes]])))
    list(
      quo = quo, data = data, label = labels[[aes]],
      owner = if (plot_owned) "plot" else "layer",
      layer_index = if (plot_owned) NA_integer_ else source$layer_index
    )
  }

  list(
    mapping = mapping, data = data, labels = labels, variables = variables,
    aesthetics = variables[aes_names], facets = facets, axes = axes,
    pins = pins, resolve_aes = resolve_aes, source = source
  )
}

#' Supply only the sibling-layer facts ordinary inheritance cannot supply
#'
#' Plot-owned mappings and data remain inherited, so replacing plot data or
#' adding aes() after an annotation still has ggplot2's normal effect.
#' @noRd
source_layer_binding <- function(plot, mapping = NULL, data = NULL,
                                 inherit.aes = TRUE) {
  source <- plot_source(plot, resolve.data = FALSE)$layer
  if (!isTRUE(inherit.aes) || is.null(source)) {
    return(list(mapping = mapping, data = data, inherit.aes = inherit.aes))
  }
  fallback <- source$mapping
  if (!isTRUE(source$inherit.aes)) {
    inherit.aes <- FALSE
  }
  mapping <- source_mapping(mapping, fallback, drop.null = FALSE)
  if (is.null(data) || inherits(data, "waiver")) {
    data <- get("data", envir = source)
    if (inherits(data, "waiver")) data <- NULL
  }
  list(mapping = mapping, data = data, inherit.aes = inherit.aes)
}

#' Defer source inheritance until a layer is added to its plot
#' @noRd
source_layer <- function(layer, inherit.data = TRUE) {
  attr(layer, "coursekata_source_data") <- inherit.data
  class(layer) <- c("coursekata_source_layer", class(layer))
  layer
}

#' Give a source callback one reproducible selection without spending RNG
#'
#' The callback still runs on the current plot data. Both the observation layer
#' and its annotation therefore see the same sample, including after new plot
#' data is supplied. No mutation reaches the caller's original layer.
#' @noRd
stabilize_source_data <- function(plot) {
  if (!inherits(plot, "ggplot")) return(plot)
  index <- plot_source_index(plot)
  if (is.na(index)) return(plot)
  layer <- plot$layers[[index]]
  callback <- layer$data
  if (!is.function(callback) ||
      (isTRUE(attr(layer, "coursekata_source_seed")) &&
        identical(get("data", envir = layer), attr(layer, "coursekata_source_data_binding")))) {
    return(plot)
  }
  seed <- with_random_seed_restored(sample.int(.Machine$integer.max, 1L))
  stable <- function(data) with_fixed_seed(seed, callback(data))
  layer <- layer_with(layer, data = stable)
  attr(layer, "coursekata_source_seed") <- TRUE
  attr(layer, "coursekata_source_data_binding") <- get("data", envir = layer)
  plot$layers[[index]] <- layer
  plot
}

#' @export
ggplot_add.coursekata_source_layer <- function(object, plot, ...) {
  plot <- stabilize_source_data(plot)
  # Residual data callbacks already add predictions. Compose them after the
  # source selection instead of replacing that preparation step.
  inherit.data <- isTRUE(attr(object, "coursekata_source_data"))
  binding <- source_layer_binding(
    plot, object$mapping,
    if (inherit.data) NULL else object$data,
    object$inherit.aes
  )
  if (inherit.data && !is.null(binding$data) && is.function(object$data)) {
    prepare <- object$data
    source <- binding$data
    binding$data <- if (is.function(source)) {
      function(data) prepare(source(data))
    } else {
      function(data) prepare(source)
    }
  } else if (inherit.data && is.null(binding$data)) {
    binding$data <- object$data
  }
  layer <- layer_with(object, mapping = binding$mapping, data = binding$data,
                      inherit.aes = binding$inherit.aes)
  class(layer) <- setdiff(class(layer), "coursekata_source_layer")
  ggplot2::ggplot_add(layer, plot, ...)
}
