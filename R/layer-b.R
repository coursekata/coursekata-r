#' Annotate model coefficients with ggplot2
#'
#' Add an intercept and a rise-over-run triangle, or arrows for group
#' differences. Marks use tagged segment, point, and text layers.
#' See [gf_b()] for the mark geometry and appearance controls.
#'
#' @details
#' This constructor returns a composite annotation. On addition it selects the
#' observation source and model layer; its component stats calculate the marks
#' when the plot builds. The model stat supplies its prediction grid once per
#' build. Adding the annotation does not evaluate data callbacks or fit a model.
#' There is no separate `stat_b()` entry point because an ordinary stat
#' cannot select a sibling model layer or compose the different drawing geoms.
#'
#' A supplied model keeps its fitted coefficients. With no model or local
#' source, the annotation uses the plotted model, or computes an implied model
#' through [StatModel] when no model layer is present. An explicit `mapping`
#' or `data` selects the annotation's own source instead of a sibling model.
#' Scale transformations and hard
#' limits then affect the inferred fit just as they affect [geom_model()].
#' `run` and `run_x` use those transformed units for an inferred model.
#' Scales and limits can be added before or after the annotation. Coordinate
#' zooming does not change the fitted rows. Replacing plot data updates inferred
#' coefficients and annotation placement; a supplied fit keeps its coefficients.
#'
#' Inference requires one model in one panel. Pass a fitted model to annotate
#' a faceted plot. A nonlinear curve has no single slope to annotate. A plotted
#' model must use the identity position: a positional adjustment moves its
#' marks away from the fitted claim.
#'
#' @param model A model fit by [stats::lm()] or [stats::aov()], or `NULL` to use
#'   the plot's model.
#' @param mapping,data Optional observation mapping and data. These select the
#'   source for this annotation without changing the other layers' source.
#'   With inherited data, the annotation reuses the visible source's resolved
#'   rows and matching positional mappings. Explicit `data` is independent and
#'   can also be a function of the plot data.
#' @param color,label_color,label_size,arrow_linewidth,show_b0,run,run_x See [gf_b()].
#' @param b0_alpha,b0_linewidth,b0_size,arrow_nudge,label_nudge See [gf_b()].
#' @param ... Unsupported appearance arguments produce a warning. British
#'   spellings `colour` and `label_colour` are accepted.
#' @param inherit.aes Inherit the observation source's mappings.
#' @return A composite annotation to add to a ggplot.
#' @export
#' @examples
#' fit <- lm(Thumb ~ Height, data = Fingers)
#' ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
#'   ggplot2::geom_point() + geom_model(model = fit) + geom_b(model = fit)
geom_b <- function(mapping = NULL, data = NULL, ..., model = NULL,
                   color = "#b599ed", label_color = "black", label_size = 3.5,
                   arrow_linewidth = 0.5, show_b0 = TRUE, run = NULL, run_x = NULL,
                   b0_alpha = 0.3, b0_linewidth = 0.8, b0_size = 4,
                   arrow_nudge = 0.18, label_nudge = 0.08, inherit.aes = TRUE) {
  dots <- list(...)
  color <- dots$colour %||% color
  label_color <- dots$label_colour %||% label_color
  b_warn_unreachable(dots[setdiff(names(dots), c("colour", "label_colour"))],
                        NA, "geom_b")
  b_annotation(model, list(
    color = color, label_color = label_color, label_size = label_size,
    arrow_linewidth = arrow_linewidth, show_b0 = show_b0, run = run, run_x = run_x,
    b0_alpha = b0_alpha, b0_linewidth = b0_linewidth, b0_size = b0_size,
    arrow_nudge = arrow_nudge, label_nudge = label_nudge
  ), mapping = mapping, data = data, inherit.aes = inherit.aes,
  fn = "geom_b", call = environment())
}

#' The shared native coefficient constructor
#' @noRd
b_annotation <- function(model, args, mapping = NULL, data = NULL,
                         inherit.aes = TRUE, fn = "geom_b", call = caller_env()) {
  structure(list(model = model, args = args, mapping = mapping, data = data,
                 inherit.aes = inherit.aes, fn = fn, call = call),
            class = "coursekata_b_layer")
}

#' @export
ggplot_add.coursekata_b_layer <- function(object, plot, ...) {
  local_source <- !is.null(object$mapping) || !is.null(object$data) ||
    !isTRUE(object$inherit.aes)
  shared_source <- local_source && is.null(object$data)
  source_index <- plot_source_index(plot)
  if (local_source) {
    plot <- stabilize_source_data(plot)
    binding <- source_layer_binding(plot, object$mapping, object$data, object$inherit.aes)
    rows <- binding$data
    if (is.function(rows)) {
      callback <- rows
      rows <- function(data) callback(data)
    }
    # This temporary point layer selects the annotation's observation source.
    # It is removed before returning the plot.
    plot$layers <- c(list(ggplot2::geom_point(data = rows, mapping = binding$mapping,
                                           inherit.aes = binding$inherit.aes)), plot$layers)
  }
  spec <- b_layer_spec(plot, object$model, object$args, object$fn, object$call,
                       source_is_local = local_source)
  spec <- b_defer_spec(spec)
  if (local_source) {
    # The selected local source must still run at build time, but is not drawn.
    local <- spec$plot$layers[[1L]]
    local <- tag_layer(layer_with(local, geom = ggplot2::GeomBlank), "b_local_source")
    spec$plot$layers <- spec$plot$layers[-1L]
    if (shared_source && !is.na(source_index)) {
      visible <- spec$plot$layers[[source_index]]
      visible <- b_seeded_source(visible, object$fn, object$call)
      shared <- b_share_source(visible, local)
      spec$plot$layers[[source_index]] <- shared$source
      local <- shared$consumer
      # compute_aesthetics runs in layer order. The visible source must resolve
      # first; moving only an invisible layer does not change drawing order.
      spec$plot$layers <- append(spec$plot$layers, list(local), after = source_index)
    } else {
      spec$plot$layers <- c(list(local), spec$plot$layers)
    }
  }
  spec$plot + spec$marks
}
