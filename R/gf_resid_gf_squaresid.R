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
#' @importFrom ggformula layer_factory
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
gf_resid <- ggformula::layer_factory(
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
  note = "the model to measure: a fit from lm() or aov()",
  # `pre` is evaluated in the ggformula namespace, so a coursekata helper needs :::
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
      # Freeze first, and assign back: the generated body ends with
      # `p <- object + new_layer`, so this is the plot the caller gets, and
      # `source_position()` below has to read the SEEDED copy. `freeze_jitter()`
      # installs a new ggproto in place of the unseeded one, so a position taken
      # before this line replays a jitter the points were never drawn with.
      object <- coursekata:::freeze_jitter(if (missing(object)) NULL else object)

      # One call, so the order the refusals fire in lives in one place: not a
      # plot, no model, no x/y on the plot, then the prediction, then the axis
      # the outcome is on. That is the order the bespoke function's lazy
      # arguments forced, and the recorded refusal messages depend on it.
      resid <- coursekata:::resid_spec(
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
      position <- coursekata:::position_resid(coursekata:::source_position(object))

      # set here rather than at the factory, both because the layer function
      # needs the mapping this call computed and because a factory-level
      # `layer_fun` is called while the package is being built, which would tie
      # this file's collation order to `geom-resid.R`'s
      layer_fun <- coursekata:::resid_layer_fun("resid", resid$aesthetics)
    }
  }
)

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
#' @importFrom ggformula layer_factory
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
gf_square_resid <- ggformula::layer_factory(
  # package-qualified so `::` resolves it whether or not coursekata is attached;
  # see the note above `gf_squareplot()`'s `layer_factory()` call
  geom = coursekata::GeomSquareResid,
  stat = coursekata::StatResid,
  # a placeholder; `pre` swaps in the position the observations are already drawn with
  position = "identity",
  # `gf_square_resid()` measures a model, not an aesthetic formula. See
  # `gf_resid()` above; NULL is what makes a bare call print
  # "gf_square_resid() does not require a formula."
  aes_form = NULL,
  # FALSE, where `gf_resid()` is TRUE, and this is a measured difference rather
  # than a copied one. Built over `gf_point(Thumb ~ Height, color = ~Sex)`,
  # `inherit = FALSE` gives every square `colour = NA` over the geom's own
  # `fill`; `inherit = TRUE` gives them the two point colors, so each square is
  # outlined in its group's color rather than reading as one neutral area.
  # `alpha` is unaffected either way -- it is set as an aes_param, which wins
  # over an inherited mapping -- so the outline is the whole of it. The bespoke
  # function passed FALSE for exactly this reason, and `resid_mapping()` names
  # it: the axes are stated outright because the squares take the geom's own
  # fill and colour. Left as the default rather than pinned in `pre` the way
  # geom/stat/position are: an outlined square is a coherent picture, just not
  # the released one.
  inherit.aes = FALSE,
  # no default on the model/function so `missing()` can tell "not supplied" from
  # "supplied as NULL"; the rest must be extras to survive the factory -- see
  # `gf_resid()` for why `...` would drop them
  extras = alist(model = , aspect = 4 / 6, alpha = 0.1),
  note = "the model to measure: a fit from lm() or aov()",
  # `pre` is evaluated in the ggformula namespace, so a coursekata helper needs :::
  pre = {
    # the second positional argument binds to `gformula`, but this function takes
    # a model there; take it back before anything reads it. NULL is gformula's real
    # default. See `gf_resid()` for what happens if either half is skipped
    if (!missing(gformula) && missing(model)) {
      model <- gformula
      gformula <- NULL
    }

    # a bare call has to reach the help gate untouched, and `show.help` is NULL
    # until ggformula decides -- see `gf_resid()` for the full reasoning
    if ((!missing(object) || !missing(model)) && !isTRUE(show.help)) {
      # Behind the help gate, because asking a function what it takes is not
      # using it, and first inside it, because a call that is about to be
      # refused for its plot or its model is still a call -- which is where the
      # bespoke function fired it.
      lifecycle::signal_stage("experimental", "gf_square_resid()")

      # freeze, and assign back: the generated body ends `p <- object + new_layer`,
      # and `source_position()` below must read the seeded copy (see `gf_resid()`)
      object <- coursekata:::freeze_jitter(if (missing(object)) NULL else object)

      # One call, so the order the refusals fire in lives in one place: not a
      # plot, no model, no x/y on the plot, then the prediction, then the axis
      # the outcome is on. `resid_spec()` returns only the data and the mapping
      # precisely so this function states its own geom, inherit and tag below.
      resid <- coursekata:::resid_spec(
        object, if (missing(model)) NULL else model, "gf_square_resid"
      )
      # only when the caller left it alone: a stray positional argument lands
      # here, and overwriting it is what would swallow the refusal ggformula
      # already makes for one
      if (missing(data)) data <- resid$data
      aesthetics <- resid$aesthetics

      # a residual is these three or it is a different picture, so state them
      # rather than leave the caller a way to swap them (see `gf_resid()`)
      geom <- coursekata::GeomSquareResid
      stat <- coursekata::StatResid
      position <- coursekata:::position_resid(coursekata:::source_position(object))

      # here rather than at the factory: it needs this call's mapping, and a
      # factory-level `layer_fun` would tie this file's collation order to geom-resid.R's
      layer_fun <- coursekata:::resid_layer_fun("square_resid", resid$aesthetics)
    }
  }
)

#' @rdname gf_square_resid
#' @description
#' `gf_squaresid()` is a fully supported alias of `gf_square_resid()`. The
#' name honors [Tyler Haslam](https://github.com/TH4SL4M), the Utah high
#' school teacher whose efforts shaped the residual and squared-residual
#' visualizations and who requested this function by that name.
#' @export
#
# Generated, not forwarded, and not a second binding of the same closure.
#
# A forwarder loses the caller's name: measured, `function(...)
# gf_square_resid(...)` reports `gf_square_resid()` in all six refusals, in the
# bare-call help header and in the experimental signal, and its
# `environment = parent.frame()` resolves to the forwarder's own frame, so
# `gf_squaresid(p, m, color = ~local_variable)` written inside a function fails
# to build.
#
# `gf_squaresid <- gf_square_resid` -- one closure, two names -- does behave
# identically, because `layer_factory()`'s body already reads the help header
# off `match.call()`. Making the refusals name the alias too then needs the
# call's own frame read out of `sys.frames()` inside `pre`, and the natural
# spelling for that, `environment()`, is a trap: R resolves a call to
# `environment()` by forcing the formal of the same name, whose default
# `parent.frame()` then evaluates inside ggformula's `eval(pre)` and answers
# with the wrong frame. Measured, that silently moves every mapped aesthetic's
# lookup into ggformula's internals -- `color = ~local_variable` stops
# resolving -- with no error until the plot is built. Generating the alias
# instead needs no stack introspection at all, and keeps this file's two
# refusal names literal the way `gf_resid()`'s is.
#
# THE PRICE IS DRIFT, AND IT IS PAID BY A TEST. Everything below is
# `gf_square_resid()`'s factory call with one string changed, and
# `test-gf_resid_gf_squaresid.R` asserts exactly that by comparing every
# argument `layer_factory()` stored on the two closures, `pre` included, after
# a mechanical rename -- so an edit made to one and not the other fails the
# suite rather than reaching a reader.
gf_squaresid <- ggformula::layer_factory(
  geom = coursekata::GeomSquareResid,
  stat = coursekata::StatResid,
  position = "identity",
  aes_form = NULL,
  inherit.aes = FALSE,
  extras = alist(model = , aspect = 4 / 6, alpha = 0.1),
  note = "the model to measure: a fit from lm() or aov()",
  pre = {
    if (!missing(gformula) && missing(model)) {
      model <- gformula
      gformula <- NULL
    }

    if ((!missing(object) || !missing(model)) && !isTRUE(show.help)) {
      lifecycle::signal_stage("experimental", "gf_squaresid()")

      object <- coursekata:::freeze_jitter(if (missing(object)) NULL else object)

      resid <- coursekata:::resid_spec(
        object, if (missing(model)) NULL else model, "gf_squaresid"
      )
      # only when the caller left it alone: a stray positional argument lands
      # here, and overwriting it is what would swallow the refusal ggformula
      # already makes for one
      if (missing(data)) data <- resid$data
      aesthetics <- resid$aesthetics

      geom <- coursekata::GeomSquareResid
      stat <- coursekata::StatResid
      position <- coursekata:::position_resid(coursekata:::source_position(object))

      layer_fun <- coursekata:::resid_layer_fun("square_resid", resid$aesthetics)
    }
  }
)
