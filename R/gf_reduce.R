#' Add Reduction Lines to a Plot
#'
#' Draws reduction lines from the grand mean to the values a fitted model
#' predicts for each observation -- the third side of the sum-of-squares
#' decomposition, alongside [gf_resid()]'s residuals and the model's own fit.
#' Each line runs along whichever axis the plot puts the model's outcome on,
#' so a model of the variable drawn on x is measured across x rather than
#' down y.
#'
#' The grand mean is the model's own, `mean()` of the outcome column the model
#' was fit on, not anything read off the plot's data. On a faceted plot that
#' is one number for every panel: every panel is measured against the same
#' line, which is what makes the picture in each panel a piece of one
#' decomposition rather than a decomposition of its own.
#'
#' @param object A ggformula plot object, typically created with `gf_point()`.
#' @param model A model already fit by [`lm()`] or [`aov()`]. The plot supplies
#'   the observations' position on the other axis; the model supplies what it
#'   predicted for each of them. May be given positionally or as `model =`. A
#'   fit without an intercept, or one fit with weights, is refused: a reduction
#'   is only a reduction because total, error and reduction add up, and that
#'   identity is what an unweighted intercept guarantees. [gf_resid()] measures
#'   either of those fits happily, needing no such identity.
#' @param linewidth The width of the reduction lines. Default is `0.2`. Must be named.
#' @param gformula Not used. `gf_reduce()` measures a model, not an aesthetic
#'   formula; a model given positionally lands here and is moved to `model`.
#' @param data Not used. The reductions are measured over the data the plot was
#'   built from. Anything supplied here is left for ggformula and ggplot2 to
#'   answer, exactly as it is for any other `gf_` layer.
#' @param ... Additional arguments. Typically these are (a) ggplot2 aesthetics to be set with
#'   `attribute = value`, such as `color`, `alpha` or `linetype`, (b) ggplot2 aesthetics to be
#'   mapped with `attribute = ~ expression`, or (c) attributes of the layer as a whole.
#' @param xlab,ylab,title,subtitle,caption Labels for the plot.
#' @param geom,stat,position Not set by the caller. A reduction is drawn by its
#'   own geom and stat, with a jitter that holds the outcome axis still so its
#'   segments start at the grand mean without floating off it, while jittering
#'   the other axis exactly the points layer's own jitter did.
#' @param show.legend Whether this layer contributes to the legend.
#' @param show.help Print the layer's own help instead of drawing.
#' @param inherit Whether the layer inherits the plot's aesthetics. The axes and
#'   the prediction are stated outright; everything else -- a mapped `color`, for
#'   instance -- is inherited from the plot.
#' @param environment The environment mappings are resolved in.
#'
#' @return A ggplot object with reduction lines added.
#'
#' @export
#' @examples
#' set.seed(1)
#' penguins_20 <- sample(penguins, 20)
#'
#' # the reduction: how far a model's fit moves the prediction from the grand
#' # mean, for a regression model
#' flipper_model <- lm(body_mass_kg ~ flipper_length_m, data = penguins_20)
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins_20) %>%
#'   gf_model(flipper_model) %>%
#'   gf_reduce(flipper_model, color = "blue")
#'
#' # and for a two-group model on a jitter plot
#' gentoo_model <- lm(body_mass_kg ~ gentoo, data = penguins_20)
#' gf_jitter(body_mass_kg ~ gentoo, data = penguins_20, width = .1) %>%
#'   gf_model(gentoo_model) %>%
#'   gf_reduce(gentoo_model, color = "blue")
#'
#' # residual (firebrick) and reduction (blue) together decompose each
#' # observation's distance from the grand mean
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins_20) %>%
#'   gf_model(flipper_model) %>%
#'   gf_resid(flipper_model, color = "firebrick") %>%
#'   gf_reduce(flipper_model, color = "blue")
gf_reduce <- named_layer_factory(
  function_name = "gf_reduce",
  # A bare ggproto symbol here only resolves through the search path -- see the
  # note above `gf_squareplot()`'s `layer_factory()` call -- so both are
  # package-qualified, which `::` resolves the same whether or not `coursekata`
  # is attached.
  geom = coursekata::GeomResid,
  stat = coursekata::StatResid,
  # a placeholder: `pre` replaces this with the outcome-holding jitter the
  # points layer is already drawn with
  position = "identity",
  # `gf_reduce()` measures a model, not an aesthetic formula: the axes come off
  # the plot and the end aesthetic is chosen by `resid_end()`. NULL is what
  # `gf_resid()` uses for the same reason, and what makes a bare call print
  # "gf_reduce() does not require a formula."
  aes_form = NULL,
  # `model` is declared with no default so that base `missing(model)` in `pre`
  # can tell "not supplied" from "supplied as NULL" -- see `gf_resid()`
  extras = alist(model = , linewidth = 0.2),
  .pre_bindings = alist(
    reduce_spec = reduce_spec,
    resid_jitter = resid_jitter,
    resid_layer_fun = resid_layer_fun
  ),
  note = "the complex model to measure: a fit from lm() or aov()",
  pre = {
    # `layer_factory()` binds the second positional argument to `gformula`, but
    # `gf_reduce()` takes a model there -- see `gf_resid()` for the full
    # reasoning, word for word the same here
    if (!missing(gformula) && missing(model)) {
      model <- gformula
      gformula <- NULL
    }

    # `pre` runs ahead of the help gate on every supported release, so a bare
    # `gf_reduce()` has to fall straight through to it -- see `gf_resid()`
    if ((!missing(object) || !missing(model)) && !isTRUE(show.help)) {
      # One call, so the order the refusals fire in lives in one place -- see
      # `reduce_spec()` for what that order is and why it matches `resid_spec()`'s.
      # It does not read the points layer's position, so calling it before the
      # jitter below is decided is safe.
      reduce <- reduce_spec(
        if (missing(object)) NULL else object, if (missing(model)) NULL else model, "gf_reduce"
      )

      # A reduction's segments start at the grand mean, a single repeated
      # number, so there is nothing for the OUTCOME axis to gain from
      # jittering -- but which physical axis that is depends on the plot's
      # orientation (a model of x is measured across x, per this function's
      # own docs). `outcome` holds that axis still while the other axis still
      # draws first, exactly as the points layer's, so its offsets stay
      # identical to the points layer's on either orientation.
      axis <- if ("xend" %in% names(reduce$aesthetics)) "x" else "y"
      jitter <- resid_jitter(if (missing(object)) NULL else object, outcome = axis)
      object <- jitter$plot

      # only when the caller left it alone: a stray positional argument lands
      # here, and overwriting it is what would swallow the refusal ggformula
      # already makes for one
      if (missing(data)) data <- reduce$data
      aesthetics <- reduce$aesthetics

      # a reduction is these three or it is a different picture -- see `gf_resid()`
      geom <- coursekata::GeomResid
      stat <- coursekata::StatResid
      position <- jitter$position

      # set here rather than at the factory -- see `gf_resid()` for why
      layer_fun <- resid_layer_fun("reduce", reduce$aesthetics)
    }
  }
)

# The squared-reduction names share one recipe but remain independently
# generated front doors so their diagnostics and caller environments stay
# literal to the name used at the call site.
gf_square_reduce_layer_factory <- function(function_name) {
  named_layer_factory(
    function_name = function_name,
    geom = coursekata::GeomSquareResid,
    stat = coursekata::StatResid,
    position = "identity",
    aes_form = NULL,
    inherit.aes = FALSE,
    extras = alist(model = , aspect = 4 / 6, alpha = 0.1),
    .pre_bindings = alist(
      reduce_spec = reduce_spec,
      resid_jitter = resid_jitter,
      resid_layer_fun = resid_layer_fun
    ),
    note = "the complex model to measure: a fit from lm() or aov()",
    pre = quote({
      if (!missing(gformula) && missing(model)) {
        model <- gformula
        gformula <- NULL
      }

      if ((!missing(object) || !missing(model)) && !isTRUE(show.help)) {
        lifecycle::signal_stage(
          "experimental", paste0(.coursekata_function_name, "()")
        )

        reduce <- reduce_spec(
          if (missing(object)) NULL else object,
          if (missing(model)) NULL else model,
          .coursekata_function_name
        )

        axis <- if ("xend" %in% names(reduce$aesthetics)) "x" else "y"
        jitter <- resid_jitter(
          if (missing(object)) NULL else object, outcome = axis
        )
        object <- jitter$plot

        if (missing(data)) data <- reduce$data
        aesthetics <- reduce$aesthetics

        geom <- coursekata::GeomSquareResid
        stat <- coursekata::StatResid
        position <- jitter$position

        layer_fun <- resid_layer_fun("square_reduce", reduce$aesthetics)
      }
    })
  )
}

#' Add Squared Reduction Visualization to a Plot
#'
#' `r lifecycle::badge("experimental")`
#'
#' Draws squared reduction polygons between the grand mean and the values a
#' fitted model predicts, so the model's share of the sum of squares is an
#' area you can see. The square is built on the reduction itself and turns
#' with it: a model of the variable the plot puts on x squares the horizontal
#' distance. Its side is scaled to stay square on the page rather than in data
#' units.
#'
#' `aspect` belongs to all three square layers or to none of them. The squared
#' reduction plus the squared residual equaling the squared total is a claim
#' about areas on the page, and it only holds while [gf_square_resid()],
#' [gf_square_reduce()] and any squared total drawn alongside them all read the
#' same `aspect`.
#'
#' @param object A ggformula plot object, typically created with `gf_point()`.
#' @param model A model already fit by [`lm()`] or [`aov()`]. The plot supplies
#'   the observations' position on the other axis; the model supplies what it
#'   predicted for each of them. May be given positionally or as `model =`. A
#'   fit without an intercept, or one fit with weights, is refused: a reduction
#'   is only a reduction because total, error and reduction add up, and that
#'   identity is what an unweighted intercept guarantees. [gf_resid()] measures
#'   either of those fits happily, needing no such identity.
#' @param aspect The square's aspect ratio. Default is `4/6`. Must be named.
#' @param alpha The transparency of the square's fill. Default is `0.1`. Must be named.
#' @param gformula Not used. `gf_square_reduce()` measures a model, not an
#'   aesthetic formula; a model given positionally lands here and is moved to
#'   `model`.
#' @param data Not used. The reductions are measured over the data the plot was
#'   built from. Anything supplied here is left for ggformula and ggplot2 to
#'   answer, exactly as it is for any other `gf_` layer.
#' @param ... Additional arguments. Typically these are (a) ggplot2 aesthetics to be set with
#'   `attribute = value`, such as `color` or `fill`, (b) ggplot2 aesthetics to be mapped with
#'   `attribute = ~ expression`, or (c) attributes of the layer as a whole.
#' @param xlab,ylab,title,subtitle,caption Labels for the plot.
#' @param geom,stat,position Not set by the caller. A squared reduction is drawn
#'   by its own geom and stat, with a jitter that holds the outcome axis still
#'   so its squares start at the grand mean without floating off it, while
#'   jittering the other axis exactly the points layer's own jitter did.
#' @param show.legend Whether this layer contributes to the legend.
#' @param show.help Print the layer's own help instead of drawing.
#' @param inherit Whether the layer inherits the plot's aesthetics. `FALSE`,
#'   where [gf_reduce()] is `TRUE` -- see [gf_square_resid()] for why: a square
#'   is a filled region drawn in the geom's own colors, and inheriting a plot's
#'   mapped `color` would outline every square in the color of the group it
#'   measures instead of leaving one neutral area per observation. Set it to
#'   `TRUE` to take the outline anyway.
#' @param environment The environment mappings are resolved in.
#'
#' @return A ggplot object with squared reduction polygons added.
#'
#' @export
#' @examples
#' set.seed(1)
#' penguins_20 <- sample(penguins, 20)
#'
#' # two squares of one decomposition: the squared residual (firebrick) and the
#' # squared reduction (blue), both reading the same aspect so the areas mean
#' # what they say
#' flipper_model <- lm(body_mass_kg ~ flipper_length_m, data = penguins_20)
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins_20) %>%
#'   gf_model(flipper_model) %>%
#'   gf_square_resid(flipper_model, color = "firebrick") %>%
#'   gf_square_reduce(flipper_model, color = "blue")
#'
#' # and for a two-group model on a jitter plot
#' gentoo_model <- lm(body_mass_kg ~ gentoo, data = penguins_20)
#' gf_jitter(body_mass_kg ~ gentoo, data = penguins_20, width = .1) %>%
#'   gf_model(gentoo_model) %>%
#'   gf_square_reduce(gentoo_model, color = "blue")
gf_square_reduce <- gf_square_reduce_layer_factory("gf_square_reduce")

#' @rdname gf_square_reduce
#' @description
#' `gf_squareduce()` is a fully supported alias of `gf_square_reduce()`, named
#' the way the classroom that asked for it says it.
#' @export
#
# Generated from the shared recipe rather than forwarded, so the alias retains
# its own diagnostic name and its caller's mapping environment.
gf_squareduce <- gf_square_reduce_layer_factory("gf_squareduce")
