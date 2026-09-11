#' Add Residual Lines to a Plot
#'
#' Draws residual lines from observed points to the values a fitted model
#' predicts for them. Each residual runs along whichever axis the plot puts the
#' model's outcome on, so a model of the variable drawn on x is measured across
#' x rather than down y.
#'
#' @param object A ggformula plot object, typically created with `gf_point()`.
#' @param model A model already fit by [`lm()`] or [`aov()`]. The plot supplies
#'   the observations; the model supplies what it predicted for each of them.
#'   May be given positionally or as `model =`.
#' @param linewidth The width of the residual lines. Default is `0.2`. Must be named.
#' @param gformula Not used. `gf_resid()` measures a model, not an aesthetic
#'   formula; a model given positionally lands here and is moved to `model`.
#' @param data Not used. The residuals are measured over the data the plot was
#'   built from. Anything supplied here is left for ggformula and ggplot2 to
#'   answer, exactly as it is for any other `gf_` layer.
#' @param ... Additional arguments. Typically these are (a) ggplot2 aesthetics to be set with
#'   `attribute = value`, such as `color`, `alpha` or `linetype`, (b) ggplot2 aesthetics to be
#'   mapped with `attribute = ~ expression`, or (c) attributes of the layer as a whole.
#' @param xlab,ylab,title,subtitle,caption Labels for the plot.
#' @param geom,stat,position Not set by the caller. A residual is drawn by its
#'   own geom and stat, and moved by the position the observations are already
#'   drawn with, so that a segment stays on the point it belongs to.
#' @param show.legend Whether this layer contributes to the legend.
#' @param show.help Print the layer's own help instead of drawing.
#' @param inherit Whether the layer inherits the plot's aesthetics. The axes and
#'   the prediction are stated outright; everything else -- a mapped `color`, for
#'   instance -- is inherited from the plot.
#' @param environment The environment mappings are resolved in.
#'
#' @return A ggplot object with residual lines added.
#'
#' @export
#' @examples
#' # residuals can be drawn on a full data set, but with hundreds of points
#' # the plot gets hard to read
#' flipper_model <- lm(body_mass_kg ~ flipper_length_m, data = penguins)
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins) %>%
#'   gf_model(flipper_model) %>%
#'   gf_resid(flipper_model)
#'
#' # a small sample makes the residuals much easier to see
#' set.seed(1)
#' penguins_20 <- sample(penguins, 20)
#'
#' # residuals from the empty model (in blue)
#' empty_model <- lm(body_mass_kg ~ NULL, data = penguins_20)
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins_20) %>%
#'   gf_model(empty_model) %>%
#'   gf_resid(empty_model, color = "blue")
#'
#' # residuals from a two-group model on a jitter plot (in firebrick)
#' gentoo_model <- lm(body_mass_kg ~ gentoo, data = penguins_20)
#' gf_jitter(body_mass_kg ~ gentoo, data = penguins_20, width = .1) %>%
#'   gf_model(gentoo_model) %>%
#'   gf_resid(gentoo_model, color = "firebrick")
#'
#' # residuals from a regression model (in firebrick)
#' sample_flipper_model <- lm(body_mass_kg ~ flipper_length_m, data = penguins_20)
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins_20) %>%
#'   gf_model(sample_flipper_model) %>%
#'   gf_resid(sample_flipper_model, color = "firebrick")
gf_resid <- named_layer_factory(
  function_name = "gf_resid",
  # A bare ggproto symbol here only resolves through the search path -- see the
  # note above `gf_squareplot()`'s `layer_factory()` call -- so both are
  # package-qualified, which `::` resolves the same whether or not `coursekata`
  # is attached.
  geom = coursekata::GeomResid,
  stat = coursekata::StatResid,
  # a placeholder: `pre` replaces this with the position the observations are
  # already drawn with on every call that has a plot to read it from
  position = "identity",
  # `gf_resid()` measures a model, not an aesthetic formula: the axes come off
  # the plot and the end aesthetic is chosen by `resid_end()`. NULL is what
  # `gf_model()` uses for the same reason, and what makes a bare call print
  # "gf_resid() does not require a formula."
  aes_form = NULL,
  # `model` is declared with no default so that base `missing(model)` in `pre`
  # can tell "not supplied" from "supplied as NULL". `linewidth` has to be an
  # extra rather than ride in on `...`: `create_extras_and_dots()` deletes every
  # formal that is not a geom formal, a stat formal or an extra, and a ggproto
  # geom has no formals at all, so `names(extras)` is the only thing protecting
  # it -- and a formal's default is the only default the factory ever applies.
  extras = alist(model = , linewidth = 0.2),
  .pre_bindings = alist(
    resid_jitter = resid_jitter,
    resid_spec = resid_spec,
    resid_layer_fun = resid_layer_fun
  ),
  note = "the model to measure: a fit from lm() or aov()",
  pre = {
    # `layer_factory()` binds the second positional argument to `gformula`, but
    # `gf_resid()` takes a model there, not an aesthetic formula, and every
    # documented call writes `p %>% gf_resid(model)`. Take it back before
    # anything reads it: a fitted model left in `gformula` dies inside
    # ggformula's own formula parsing. `NULL` is `gformula`'s real default -- an
    # empty `missing_arg()` here aborts. The move is unconditional, so a model
    # written as a formula still reaches `resid_fitted()` and still fails there,
    # exactly as it did before: `gf_resid()` has never fit a model for you.
    if (!missing(gformula) && missing(model)) {
      model <- gformula
      gformula <- NULL
    }

    # `pre` runs ahead of the help gate on every supported release, so a bare
    # `gf_resid()` has to fall straight through to it. Base `missing()`, never
    # `rlang::is_missing()`, which forces the promise and kills that path;
    # `isTRUE()` because `show.help` is NULL, not FALSE, until ggformula decides
    # one. The `!missing(model)` half is what sends `gf_resid(model = m)` -- a
    # model with no plot -- to `resid_spec()`'s refusal rather than letting
    # ggformula build a new empty plot around the layer.
    if ((!missing(object) || !missing(model)) && !isTRUE(show.help)) {
      # The residual has to start where its point is drawn, and the point may be
      # jittered. Read that jitter and declare the same one on this layer rather
      # than replaying the plot's: two layers sharing a seed land identically.
      # An unseeded jitter has no offsets to share, so it is pinned here -- on
      # the plot this returns, never on the one the caller still holds.
      jitter <- resid_jitter(if (missing(object)) NULL else object)
      object <- jitter$plot

      # One call, so the order the refusals fire in lives in one place: not a
      # plot, no model, no x/y on the plot, then the prediction, then the axis
      # the outcome is on. That is the order the bespoke function's lazy
      # arguments forced, and the recorded refusal messages depend on it.
      resid <- resid_spec(
        object, if (missing(model)) NULL else model, "gf_resid"
      )
      # only when the caller left it alone: a stray positional argument lands
      # here, and overwriting it is what would swallow the refusal ggformula
      # already makes for one
      if (missing(data)) data <- resid$data
      aesthetics <- resid$aesthetics

      # The generated signature carries `geom`, `stat` and `position`, which the
      # bespoke function never did. A residual is these three or it is a
      # different picture -- `geom = "segment"` draws the ends where they arrive
      # instead of transposing them, and moves the drawing without moving the
      # built data -- so state them here rather than leave the caller a way to
      # swap them. Assignments in `pre` shadow the formals, and `eval_tidy()`
      # returns a ggproto unchanged, which is how `gf_model()` sets its geom.
      geom <- coursekata::GeomResid
      stat <- coursekata::StatResid
      position <- jitter$position

      # set here rather than at the factory, both because the layer function
      # needs the mapping this call computed and because a factory-level
      # `layer_fun` is called while the package is being built, which would tie
      # this file's collation order to `geom-resid.R`'s
      layer_fun <- resid_layer_fun("resid", resid$aesthetics)
    }
  }
)

# Generate the two squared-residual names from one configuration while keeping
# separate closures, literal diagnostics, and direct caller environments.
gf_square_resid_layer_factory <- function(function_name) {
  named_layer_factory(
    function_name = function_name,
    geom = coursekata::GeomSquareResid,
    stat = coursekata::StatResid,
    position = "identity",
    aes_form = NULL,
    inherit.aes = FALSE,
    extras = alist(model = , aspect = 4 / 6, alpha = 0.1),
    .pre_bindings = alist(
      resid_jitter = resid_jitter,
      resid_spec = resid_spec,
      resid_layer_fun = resid_layer_fun
    ),
    note = "the model to measure: a fit from lm() or aov()",
    pre = quote({
      if (!missing(gformula) && missing(model)) {
        model <- gformula
        gformula <- NULL
      }

      if ((!missing(object) || !missing(model)) && !isTRUE(show.help)) {
        lifecycle::signal_stage(
          "experimental", paste0(.coursekata_function_name, "()")
        )

        jitter <- resid_jitter(if (missing(object)) NULL else object)
        object <- jitter$plot

        resid <- resid_spec(
          object,
          if (missing(model)) NULL else model,
          .coursekata_function_name
        )
        if (missing(data)) data <- resid$data
        aesthetics <- resid$aesthetics

        geom <- coursekata::GeomSquareResid
        stat <- coursekata::StatResid
        position <- jitter$position

        layer_fun <- resid_layer_fun("square_resid", resid$aesthetics)
      }
    })
  )
}

#' Add Squared Residual Visualization to a Plot
#'
#' `r lifecycle::badge("experimental")`
#'
#' Draws squared residual polygons between observed points and the values a
#' fitted model predicts for them, so squared error is an area you can see. The
#' square is built on the residual itself and turns with it: a model of the
#' variable the plot puts on x squares the horizontal distance. Its side is
#' scaled to stay square on the page rather than in data units.
#'
#' @param object A ggformula plot object, typically created with `gf_point()`.
#' @param model A model already fit by [`lm()`] or [`aov()`]. The plot supplies
#'   the observations; the model supplies what it predicted for each of them.
#'   May be given positionally or as `model =`.
#' @param aspect The square's aspect ratio. Default is `4/6`. Must be named.
#' @param alpha The transparency of the square's fill. Default is `0.1`. Must be named.
#' @param gformula Not used. `gf_square_resid()` measures a model, not an
#'   aesthetic formula; a model given positionally lands here and is moved to
#'   `model`.
#' @param data Not used. The residuals are measured over the data the plot was
#'   built from. Anything supplied here is left for ggformula and ggplot2 to
#'   answer, exactly as it is for any other `gf_` layer.
#' @param ... Additional arguments. Typically these are (a) ggplot2 aesthetics to be set with
#'   `attribute = value`, such as `color` or `fill`, (b) ggplot2 aesthetics to be mapped with
#'   `attribute = ~ expression`, or (c) attributes of the layer as a whole.
#' @param xlab,ylab,title,subtitle,caption Labels for the plot.
#' @param geom,stat,position Not set by the caller. A squared residual is drawn
#'   by its own geom and stat, and moved by the position the observations are
#'   already drawn with, so that a square stays on the point it belongs to.
#' @param show.legend Whether this layer contributes to the legend.
#' @param show.help Print the layer's own help instead of drawing.
#' @param inherit Whether the layer inherits the plot's aesthetics. `FALSE`,
#'   where [gf_resid()] is `TRUE`: a square is a filled region drawn in the
#'   geom's own colors, so inheriting a plot's mapped `color` outlines every
#'   square in the color of the group it measures instead of leaving one
#'   neutral area per observation. The axes and the prediction are stated
#'   outright, so nothing the square needs is lost by not inheriting. Set it to
#'   `TRUE` to take the outline anyway.
#' @param environment The environment mappings are resolved in.
#'
#' @return A ggplot object with squared residual polygons added.
#'
#' @export
#' @examples
#' # squared residuals can be drawn on a full data set, but with hundreds of
#' # points the plot gets hard to read
#' flipper_model <- lm(body_mass_kg ~ flipper_length_m, data = penguins)
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins) %>%
#'   gf_model(flipper_model) %>%
#'   gf_square_resid(flipper_model)
#'
#' # a small sample makes the squared residuals much easier to see
#' set.seed(1)
#' penguins_20 <- sample(penguins, 20)
#'
#' # squared residuals from the empty model (in blue)
#' empty_model <- lm(body_mass_kg ~ NULL, data = penguins_20)
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins_20) %>%
#'   gf_model(empty_model) %>%
#'   gf_square_resid(empty_model, color = "blue")
#'
#' # squared residuals from a two-group model on a jitter plot (in firebrick)
#' gentoo_model <- lm(body_mass_kg ~ gentoo, data = penguins_20)
#' gf_jitter(body_mass_kg ~ gentoo, data = penguins_20, width = .1) %>%
#'   gf_model(gentoo_model) %>%
#'   gf_square_resid(gentoo_model, color = "firebrick")
#'
#' # squared residuals from a regression model (in firebrick)
#' sample_flipper_model <- lm(body_mass_kg ~ flipper_length_m, data = penguins_20)
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins_20) %>%
#'   gf_model(sample_flipper_model) %>%
#'   gf_square_resid(sample_flipper_model, color = "firebrick")
gf_square_resid <- gf_square_resid_layer_factory("gf_square_resid")

#' @rdname gf_square_resid
#' @description
#' `gf_squaresid()` is a fully supported alias of `gf_square_resid()`. The
#' name honors [Tyler Haslam](https://github.com/TH4SL4M), the Utah high
#' school teacher whose efforts shaped the residual and squared-residual
#' visualizations and who requested this function by that name.
#' @export
#
# Generated from the shared recipe rather than forwarded or rebound, preserving
# the alias's literal name and the caller frame used for mapped aesthetics.
gf_squaresid <- gf_square_resid_layer_factory("gf_squaresid")
