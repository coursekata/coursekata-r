#' Add Reduction Lines to a Plot
#'
#' Draws reduction lines from the grand mean to the value a fitted model
#' predicts for each observation. Squaring and summing those lengths across
#' observations gives the model sum of squares; [gf_resid()] supplies the error
#' term in the same decomposition.
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
#'   fit without an intercept, or one fit with weights, is refused. For an
#'   unweighted least-squares fit with an intercept, the sum of squared total
#'   deviations equals the sum of squared residuals plus the sum of squared
#'   reductions. That identity need not hold without an intercept. Weighted
#'   least squares instead guarantees a weighted identity based on a weighted
#'   mean, which the plot's plain areas do not represent. [gf_resid()] can
#'   measure either fit because it does not rely on the decomposition.
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
#' # each observation's signed deviation from the grand mean is its residual
#' # (firebrick) plus its reduction (blue)
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins_20) %>%
#'   gf_model(flipper_model) %>%
#'   gf_resid(flipper_model, color = "firebrick") %>%
#'   gf_reduce(flipper_model, color = "blue")
gf_reduce <- named_layer_factory(
  function_name = "gf_reduce",
  # Generated functions may run without coursekata attached, so qualify the
  # ggproto objects they capture.
  geom = coursekata::GeomResid,
  stat = coursekata::StatReduce,
  # `pre` replaces this with the point layer's endpoint-preserving jitter.
  position = "identity",
  # Axes come from the plot; this layer has no aesthetic formula of its own.
  aes_form = NULL,
  # No default: `pre` distinguishes an omitted model from an explicit `NULL`.
  extras = alist(model = , linewidth = 0.2),
  .pre_bindings = alist(
    reduce_spec = reduce_spec,
    resid_jitter = resid_jitter,
    resid_layer_fun = resid_layer_fun
  ),
  note = "the complex model to measure: a fit from lm() or aov()",
  pre = {
    # `layer_factory()` binds the second positional argument to `gformula`.
    if (!missing(gformula) && missing(model)) {
      model <- gformula
      gformula <- NULL
    }

    if ((!missing(object) || !missing(model)) && !isTRUE(show.help)) {
      reduce <- reduce_spec(
        if (missing(object)) NULL else object, if (missing(model)) NULL else model, "gf_reduce"
      )

      # Keep the grand-mean axis fixed while reproducing the point layer's
      # jitter on the other axis.
      axis <- if ("xend" %in% names(reduce$aesthetics)) "x" else "y"
      jitter <- resid_jitter(if (missing(object)) NULL else object, outcome = axis)
      object <- jitter$plot

      # Preserve a caller-supplied data argument so ggformula can validate it.
      if (missing(data)) data <- reduce$data
      aesthetics <- reduce$aesthetics

      geom <- coursekata::GeomResid
      stat <- coursekata::StatReduce
      position <- jitter$position

      layer_fun <- resid_layer_fun("reduce", reduce$aesthetics)
    }
  }
)

# Generate each squared-reduction name independently so diagnostics name the
# function called and mappings keep the caller's environment.
gf_square_reduce_layer_factory <- function(function_name) {
  named_layer_factory(
    function_name = function_name,
    geom = coursekata::GeomSquareResid,
    stat = coursekata::StatReduce,
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
        stat <- coursekata::StatReduce
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
#' fitted model predicts. Each polygon shows one observation's squared
#' reduction; together, their areas represent the model sum of squares. The
#' square is built on the reduction itself and turns with it: a model of the
#' variable the plot puts on x squares the horizontal distance. Its side is
#' scaled to stay square on the page rather than in data units.
#'
#' Use the same `aspect` for all three square layers. Across observations, the
#' reduction areas and residual areas sum to the total areas. The equality is
#' between those sums, not between the three squares for any one observation.
#' [gf_square_resid()], [gf_square_reduce()], and any squared total drawn beside
#' them must use the same `aspect` for their areas to share a scale.
#'
#' @param object A ggformula plot object, typically created with `gf_point()`.
#' @param model A model already fit by [`lm()`] or [`aov()`]. The plot supplies
#'   the observations' position on the other axis; the model supplies what it
#'   predicted for each of them. May be given positionally or as `model =`. A
#'   fit without an intercept, or one fit with weights, is refused. For an
#'   unweighted least-squares fit with an intercept, the sum of squared total
#'   deviations equals the sum of squared residuals plus the sum of squared
#'   reductions. That identity need not hold without an intercept. Weighted
#'   least squares instead guarantees a weighted identity based on a weighted
#'   mean, which the plot's plain areas do not represent. [gf_resid()] can
#'   measure either fit because it does not rely on the decomposition.
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
#' # two collections of squares in one sample-level decomposition: squared
#' # residuals (firebrick) and squared reductions (blue), drawn at one aspect
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
