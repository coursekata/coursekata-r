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

#' Translate the model a plot implies into the pieces a ggformula layer needs
#'
#' Reads the shared decision (`implied_model()`) and turns it into a layer: a
#' geom/stat pair, the axes it draws (stated, never inherited -- see below),
#' and the params those stats and geoms need as a fallback when the caller's
#' own `...` does not supply them. The one param decision this makes that a
#' caller's own value can never override is `mark_axis`/`orientation` on the
#' `segment` shape: which axis carries the groups is a fact about the plot,
#' not a style choice, and `model_plan()`'s own comment makes the identical
#' argument for the explicit path.
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
#'   `aesthetics`, `params`, `inherit` and `tag`. The layer function is composed
#'   by the caller in `pre`, because the two paths have opposite param policies.
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

  geom <- switch(im$kind,
    # GeomSmooth, not GeomLine, so that `se = TRUE` draws the band this
    # function documents. StatSmooth computes `ymin`/`ymax` either way, and
    # GeomLine has nowhere to put them: the band was being computed and thrown
    # away, and a reader who asked for it got a plain line and no complaint.
    # With `se = FALSE` -- the default -- GeomSmooth draws through GeomLine
    # itself, so nothing about the ordinary picture changes except that its
    # colour now has to be stated (see `params`).
    line = ggplot2::GeomSmooth,
    segment = GeomModelMark,
    hline = ggplot2::GeomHline,
    vline = ggplot2::GeomVline
  )
  stat <- switch(im$kind,
    line = ggplot2::StatSmooth,
    segment = ggplot2::StatSummary,
    hline = stat_dist_mean("y"),
    vline = stat_dist_mean("x")
  )
  params <- switch(im$kind,
    # `formula` is named here, not left for StatSmooth's own default, so that
    # default is never inferred at build time -- inferring it is what prints
    # ggplot2's "`geom_smooth()` using formula = 'y ~ x'" note, naming a
    # function the caller never called. `implied_layer_fun()`'s modifyList()
    # still lets a caller's own `formula = y ~ poly(x, 2)` win.
    # `fullrange` is named rather than left to `stat_smooth()`'s own default,
    # even though the two agree, so that the choice is a stated one: a model's
    # line runs across the data it was fit on and no further. Inside that range
    # every interpolated point has observations bracketing it; outside there
    # are none, and only a theory or a physical constraint can license the
    # claim -- which is a judgment a reader makes, not one a plot can reach by
    # measuring its own axis. `modifyList()` still lets a reader who has made
    # that judgment pass `fullrange = TRUE` through to [ggplot2::StatSmooth].
    line = list(
      method = "lm", formula = y ~ x, se = FALSE, fullrange = FALSE, linewidth = 1,
      # GeomSmooth's own default is a blue that means "a smoother" in ggplot2's
      # vocabulary; this line means "the model", and the rest of the family
      # draws that in the neutral GeomLine uses. Same reasoning, same spelling,
      # as the `segment` branch below.
      colour = ggplot2::GeomLine$default_aes$colour
    ),
    segment = list(
      fun = mean, na.rm = TRUE, width = .4,
      # internal and authoritative: a caller cannot change which axis holds
      # groups, the same reasoning model_plan()'s own comment gives
      mark_axis = if (im$flipped) "y" else "x",
      orientation = if (im$flipped) "y" else "x",
      colour = ggplot2::GeomLine$default_aes$colour,
      linewidth = 1
    ),
    list(linewidth = 1)
  )

  list(
    plot = im$plot,
    geom = geom,
    stat = stat,
    # stated, and not a default a caller's `...` can reach. A position moves
    # what a layer drew, and what this layer drew is a claim: a fitted line
    # nudged by `position = "jitter"` is a different line every render, and one
    # the model never made. `gf_model()`'s own documentation already promises
    # that with no model the position is not read, and until this was stated
    # the promise was the only thing stopping it.
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
#' Unlike `model_layer_fun()`, which discards ggformula's own params because
#' `model_plan()` already supplied the whole set, this KEEPS them: `formula`,
#' `se`, `n`, `colour` and `linewidth` are the whole point of `...` here, and
#' there is no plan supplying them instead. `defaults` supplies a value only
#' where the caller did not -- `utils::modifyList()` lets the caller's own
#' params win -- except for `mark_axis`/`orientation`, which `implied_model_spec()`
#' computed from the plot itself and which no `...` argument may override.
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
  authoritative <- intersect(c("mark_axis", "orientation"), names(defaults))
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
    tag_layer(
      ggplot2::layer(
        geom = geom, stat = stat, position = position,
        mapping = mapping, data = data, params = merged, ...
      ),
      tag
    )
  }
}
