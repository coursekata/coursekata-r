#' Which two mapped expressions, for one aesthetic, could not be reconciled
#'
#' `pin_plot_values()` reports the aesthetic by name in `unreached`; the
#' refusal in `implied_model_spec()` needs the two spellings that disagreed, so
#' it can say what the reader actually wrote rather than just which aesthetic.
#' Read off the ORIGINAL, unpinned plot -- by the time a refusal is raised the
#' pin has already rewritten one of the two into `.coursekata_pin_*`, which is
#' not a spelling a reader wrote.
#'
#' @param object The plot as the caller passed it in, before any pin.
#' @param aes The aesthetic that could not be reached.
#'
#' @return A list with `plot` and `layer`, both single strings.
#'
#' @noRd
diverging_mapping_expressions <- function(object, aes) {
  plot_quo <- object$mapping[[aes]]
  layer_quo <- NULL
  for (layer in object$layers) {
    candidate <- layer$mapping[[aes]]
    if (!is.null(candidate) && !identical(candidate, plot_quo)) {
      layer_quo <- candidate
      break
    }
  }
  list(
    plot = if (is.null(plot_quo)) "(unmapped)" else as_label(quo_get_expr(plot_quo)),
    layer = if (is.null(layer_quo)) "(unmapped)" else as_label(quo_get_expr(layer_quo))
  )
}

#' Translate the model a plot implies into the shared model layer
#'
#' Reads the shared decision (`implied_model()`) and supplies [StatModel] and
#' [GeomModel] with the axes, orientation, and defaults they need. Orientation
#' comes from the plot rather than from `...`: which axis carries the outcome is
#' not a style choice.
#'
#' The layer states its axes rather than inheriting them, exactly as
#' `resid_mapping()` does: `inherit = FALSE`, `data` is the pinned plot's own
#' data (so facet columns travel to the stat, which fits per panel), and the
#' mapping is built from the pinned plot's own mapping for only the axes this
#' shape needs. Stating rather than inheriting is what keeps
#' `gf_point(Thumb ~ Height, color = ~Sex) %>% gf_model()` drawing ONE black
#' line rather than one per color: the inferred model is a model of the two
#' axes, not of the legend.
#'
#' @param object The plot, as the caller passed it in -- unpinned. Refusing a
#'   non-plot first argument is the caller's job, not this function's.
#' @param args Named list of user arguments, from `...`. Unused today: every
#'   fitting argument (`formula`, `se`, `n`, `colour`, `linewidth`) reaches the
#'   layer through ggformula's own params, which `implied_layer_fun()` keeps
#'   rather than discards. Kept as a formal so the two `*_spec()` functions
#'   share one call shape in `gf_model.R`'s `pre` block.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with `plot` (possibly pinned), `geom`, `stat`, `data`,
#'   `position`, `aesthetics`, `params`, `inherit` and `tag`. The layer function
#'   is composed by the caller in `pre`, because the two paths have opposite
#'   param policies.
#' @noRd
implied_model_spec <- function(object, args = list(), call = caller_env()) {
  if (!inherits(object, c("gg", "ggplot"))) {
    abort(
      c(
        "`gf_model()` needs to be layered on top of a plot.",
        i = "start one: `gf_point(Thumb ~ Height, data = Fingers) %>% gf_model()`"
      ),
      call = call
    )
  }

  im <- implied_model(object, call = call)

  if (length(im$unreached) > 0) {
    aes <- im$unreached[[1]]
    divergence <- diverging_mapping_expressions(object, aes)
    abort(
      c(
        glue(
          "`gf_model()` cannot infer a model from a plot that draws `{aes}` two different ways"
        ),
        x = paste0(
          glue("one layer maps `{divergence$plot}` and another maps `{divergence$layer}`, "),
          "so the model would be fit on something part of the plot does not show"
        ),
        i = "map it once, in `ggplot(data, aes(...))`, or name the model: `gf_model(Thumb ~ Height)`"
      ),
      call = call
    )
  }

  check_numeric_outcome(im$outcome$label, im$data[[im$outcome$column]], call)

  spec <- plot_spec(im$plot)
  check_model_axes(spec, call = call)

  axes_needed <- switch(im$kind, line = , segment = c("x", "y"), hline = "y", vline = "x")
  aesthetics <- ggplot2::aes()
  for (a in axes_needed) aesthetics[[a]] <- spec$mapping[[a]]

  geom <- GeomModel
  stat <- StatModel
  params <- switch(im$kind,
    # Keep the inferred line within the observed data range by default. The
    # caller can still supply another `formula` or set `fullrange = TRUE`.
    line = list(
      formula = y ~ x, se = FALSE, fullrange = FALSE,
      orientation = if (im$flipped) "y" else "x", linewidth = 1,
      # Resolve the themed line default now so every model representation uses
      # the same neutral colour.
      colour = ggplot2::get_geom_defaults("line")$colour
    ),
    segment = list(
      na.rm = TRUE, width = .4,
      orientation = if (im$flipped) "y" else "x",
      colour = ggplot2::get_geom_defaults("line")$colour,
      linewidth = 1
    ),
    list(
      orientation = if (im$flipped) "y" else "x",
      linewidth = 1
    )
  )

  list(
    plot = im$plot,
    geom = geom,
    stat = stat,
    # A positional adjustment would move the fitted claim away from the model,
    # so inferred layers always use the identity position.
    position = "identity",
    data = im$data,
    aesthetics = aesthetics,
    params = params,
    inherit = FALSE,
    tag = "model"
  )
}

#' The layer function for an inferred model
#'
#' The inferred path keeps ggformula's params because there is no explicit model
#' plan supplying them. `defaults` fills only missing values, except that the
#' orientation resolved from the plot remains authoritative.
#'
#' @param defaults The kind-specific fallback params `implied_model_spec()` computed.
#' @param tag The tag to name the layer with.
#'
#' @return A function with the formals `layer_factory()` expects. It must name
#'   `geom`, `stat`, `position` and `params`: a `...`-only shim is stripped of
#'   all four by `create_formals()` and fails with a missing geom.
#'
#' @noRd
implied_layer_fun <- function(defaults, tag) {
  force(defaults)
  force(tag)
  authoritative <- intersect("orientation", names(defaults))
  function(geom, stat, position, params = NULL, mapping = NULL, data = NULL, ...) {
    supplied <- params %||% list()
    # `color` and `colour` are one parameter spelled two ways, and
    # `modifyList()` matches names literally: without this, a default stated as
    # `colour` outranks the caller's own `color =` instead of being replaced by
    # it, and `gf_model(color = "firebrick")` silently draws black. The
    # explicit path normalizes the same pair at the top of `model_plan()`.
    if (!is.null(supplied$color)) {
      supplied$colour <- supplied$color
      supplied$color <- NULL
    }
    merged <- utils::modifyList(defaults, supplied)
    merged[authoritative] <- defaults[authoritative]
    orientation <- merged[["orientation"]] %||% NA
    merged[["orientation"]] <- NULL
    model_layer(
      geom = geom, stat = stat, position = position,
      mapping = mapping, data = data, params = merged,
      orientation = orientation, tag = tag, prepared = TRUE, ...
    )
  }
}
