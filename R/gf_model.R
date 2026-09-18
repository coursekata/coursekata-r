#' Add a model to a plot
#'
#' When teaching about regression it can be useful to visualize the data as a point plot with the
#' outcome on the y-axis and the explanatory variable on the x-axis. For regression models, this is
#' most easily achieved by calling [`ggformula::gf_lm()`], with empty models
#' [`ggformula::gf_hline()`] using the mean, and a more complicated call to
#' [`ggformula::gf_segment()`] for group models. This function simplifies this
#' by making a guess about what kind of model you are plotting (empty/null, regression, group) and
#' then making the appropriate plot layer for it.
#'
#' This function only works with models that have a continuous outcome measure.
#'
#' @section Supported plots:
#' `gf_model()` is built for and tested against plots made with
#' [ggformula::gf_point()], [ggformula::gf_jitter()], [ggformula::gf_boxplot()],
#' [ggformula::gf_violin()] and [ggformula::gf_histogram()]. Other plots may
#' work if they map their variables the same way, but they are not tested.
#'
#' @param object A plot created with the `ggformula` package.
#' @param model The model to draw. Either a model already fit by [`lm()`] or
#'   [`aov()`], or the formula for one -- such as `body_mass_kg ~ species` --
#'   which is fit against the data the plot was built from. The empty model is
#'   written `body_mass_kg ~ NULL`. The outcome must be named: a one-sided
#'   formula such as `~species` names predictors and no claim, so there would be
#'   nothing to draw that you had not described. May be given positionally or as
#'   `model =`. Omitted, the model the plot implies is drawn instead: a
#'   regression line for a numeric predictor, one group mean per level for a
#'   categorical one, and the grand mean when the plot draws only an outcome.
#'   `gf_model(model)` draws the ONE model you named in every panel of a faceted
#'   plot; `gf_model()` draws EACH panel's own implied model -- a faceted
#'   `gf_point(body_mass_kg ~ flipper_length_m | species) %>% gf_model()` fits a
#'   different line per species, where naming `flipper_model` above would repeat
#'   one whole-data fit in every panel.
#' @param gformula Not used. `gf_model()` draws a model, not an aesthetic
#'   formula; a model given positionally lands here and is moved to `model`.
#' @param data Not used. The layer draws the model's predictions, which are
#'   computed from the data the plot was built from.
#' @param ... Additional arguments. Typically these are (a) ggplot2 aesthetics to be set with
#'   `attribute = value`, (b) ggplot2 aesthetics to be mapped with `attribute = ~ expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'
#'   With no `model`, and a plot whose implied shape is a regression line, `...` also reaches the
#'   fitting
#'   vocabulary [`ggformula::gf_lm()`] uses: `se = TRUE` draws the confidence band `gf_lm()` calls
#'   `interval = "confidence"`, `n =` sets the prediction grid's length, and `method.args =` is
#'   `gf_lm()`'s `lm.args =`. `formula = y ~ poly(x, 2)` fits a curve, written with the literal
#'   tokens `x` and `y` rather than the plot's own variable names -- a one-sided `formula = ~x`
#'   would instead be read as a mapping, so it is not this.
#'
#'   With a supplied model, `n` controls the prediction grid and `se = TRUE`
#'   is refused. One additional predictor may be present in the data without
#'   being mapped: its categories, or its mean and mean plus or minus one
#'   standard deviation, define separate model traces.
#'
#'   [ggplot2::stat_smooth()]'s `fullrange = TRUE` reaches it too, and draws the line past the
#'   data. Whether that is honest is yours to decide and not something this function will decide
#'   for you: inside the range the model was fit on, every point the line interpolates has
#'   observations on both sides of it, and outside there are none. Extending it says the pattern
#'   keeps holding where nothing was measured, which needs a theory or a physical constraint
#'   behind it. Nothing about the plot's own axis is such a reason, which is why a wider axis --
#'   from `gf_lims()`, or from the `b0` dot `gf_b()` puts at zero -- does not lengthen the line
#'   on its own.
#'
#' @param xlab,ylab,title,subtitle,caption Labels for the plot.
#' @param geom Not set by the caller. The geometry is derived from the model.
#' @param stat,position With a named `model`, reach the layer as given, but
#'   `gf_model()` computes the model's predictions before the layer is built.
#'   Changing the stat recomputes something else on that prediction grid rather
#'   than changing the model's claim, so leave these at their defaults. With no
#'   `model`, the shared [stat_model()] is used and these are not read.
#' @param show.legend Whether this layer contributes to the legend.
#' @param show.help Print the layer's own help instead of drawing.
#' @param inherit Not set by the caller. A named intercept stands alone; other
#'   named models inherit compatible plot aesthetics while stating their own
#'   predicted outcome. With no `model`, the layer states its own positions and
#'   does not inherit a color grouping the model never named.
#' @param environment The environment mappings are resolved in.
#'
#' @return A ggplot object with the model added. With no `model`, and a plot
#'   whose positional mapping is an expression rather than a bare variable (`shuffle(body_mass_kg)`,
#'   `log(flipper_length_m)`), the RETURNED plot is pinned to the values it drew when `gf_model()`
#'   was called -- its data gains a fixed column and its mapping names it, while its axis titles
#'   and everything else about how it reads keep your own words. The plot passed IN is untouched.
#'
#' @export
#' @examples
#' # the empty model predicts the same value (the mean) for every observation
#' empty_model <- lm(body_mass_kg ~ NULL, data = penguins)
#' gf_histogram(~body_mass_kg, data = penguins, binwidth = 0.25) %>%
#'   gf_model(empty_model)
#'
#' # a two-group model (categorical explanatory variable) on a jitter plot
#' gentoo_model <- lm(body_mass_kg ~ gentoo, data = penguins)
#' gf_jitter(body_mass_kg ~ gentoo, data = penguins, width = .1) %>%
#'   gf_model(gentoo_model)
#'
#' # a three-group model works the same way
#' species_model <- lm(body_mass_kg ~ species, data = penguins)
#' gf_jitter(body_mass_kg ~ species, data = penguins, width = .1) %>%
#'   gf_model(species_model)
#'
#' # group models can also be layered onto faceted histograms
#' gf_histogram(~body_mass_kg, data = penguins, binwidth = 0.25) %>%
#'   gf_facet_grid(species ~ .) %>%
#'   gf_model(species_model)
#'
#' # a regression model (quantitative explanatory variable) on a scatter plot
#' flipper_model <- lm(body_mass_kg ~ flipper_length_m, data = penguins)
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins) %>%
#'   gf_model(flipper_model)
#'
#' # layer the empty model and the regression model in different colors to
#' # compare the two models on the same plot
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins) %>%
#'   gf_model(empty_model, color = "dodgerblue") %>%
#'   gf_model(flipper_model, color = "firebrick")
#'
#' # with a categorical and a quantitative predictor, the model is drawn
#' # as one line for each group
#' ancova_model <- lm(body_mass_kg ~ species + flipper_length_m, data = penguins)
#' gf_point(body_mass_kg ~ flipper_length_m, color = ~species, data = penguins) %>%
#'   gf_model(ancova_model)
#'
#' # a model that has not been fit yet can be written as a formula, and is fit
#' # against the data the plot was built from
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins) %>%
#'   gf_model(body_mass_kg ~ flipper_length_m)
#'
#' # the empty model, written as a formula
#' gf_histogram(~body_mass_kg, data = penguins, binwidth = 0.25) %>%
#'   gf_model(body_mass_kg ~ NULL)
#'
#' # with no model, gf_model() draws the model the plot implies: a numeric
#' # predictor draws the regression line gf_lm() would fit
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins) %>%
#'   gf_model()
#'
#' # a categorical predictor draws one mark at each group's mean
#' gf_jitter(body_mass_kg ~ species, data = penguins, width = .1) %>%
#'   gf_model()
#'
#' # a plot that draws only its outcome implies the grand mean
#' gf_histogram(~body_mass_kg, data = penguins, binwidth = 0.25) %>%
#'   gf_model()
gf_model <- named_layer_factory(
  function_name = "gf_model",
  geom = "model",
  stat = "model",
  position = "identity",
  aes_form = NULL,
  extras = alist(model = ),
  .pre_bindings = alist(
    implied_model_spec = implied_model_spec,
    model_layer_spec = model_layer_spec,
    implied_layer_fun = implied_layer_fun,
    model_layer_fun = model_layer_fun
  ),
  note = "the model to draw: a fit from lm() or aov(), or the formula for one",
  pre = {
    # `layer_factory()` binds the second positional argument to `gformula`, but
    # `gf_model()` takes a model there, not an aesthetic formula, and every
    # documented call writes `p %>% gf_model(model)`. Take it back before
    # anything reads it: a fitted model left in `gformula` dies inside
    # ggformula's own formula parsing, and a model written as a formula would be
    # read as a mapping. `NULL` is `gformula`'s real default -- an empty
    # `missing_arg()` here aborts.
    if (!missing(gformula) && missing(model)) {
      model <- gformula
      gformula <- NULL
    }

    # `pre` runs ahead of the help gate on every supported release, so a bare
    # `gf_model()` has to fall straight through to it -- that is the `missing(object)`
    # half above. An explicit `show.help = TRUE` is the same request with a plot
    # already attached: every other `gf_*` layer answers it before touching its
    # arguments, and `model_layer_spec()` would abort on a missing model before the
    # help gate downstream ever runs. Base `missing()`, never `rlang::is_missing()`,
    # which forces the promise and kills that path; `isTRUE()` because the default
    # is `NULL`, not `FALSE`, until ggformula decides one.
    if (!missing(object) && !isTRUE(show.help)) {
      inferred <- missing(model) || is.null(model)
      spec <- if (inferred) {
        implied_model_spec(object, rlang::list2(...))
      } else {
        model_layer_spec(object, model, rlang::list2(...))
      }
      object <- spec$plot %||% object # only the inferred path pins
      geom <- spec$geom
      stat <- spec$stat %||% stat
      position <- spec$position %||% position
      data <- spec$data
      aesthetics <- spec$aesthetics
      inherit <- spec$inherit
      layer_fun <- if (inferred) {
        implied_layer_fun(spec$params, spec$tag)
      } else {
        model_layer_fun(
          spec$params, spec$tag, spec$orientation, spec$aesthetics, spec$model
        )
      }
    }
  }
)

#' Build the layer function that draws a model
#'
#' Everything the caller typed has already been through `model_plan()`, which is
#' the one place that decides what a model looks like on a plot. Taking
#' ggformula's copy of those arguments as well would supply `color` beside the
#' plan's `colour` and duplicate the aesthetic.
#'
#' @param plan_params The plan's static arguments.
#' @param tag The tag to name the layer with.
#' @param orientation The orientation resolved from the ggformula plot.
#' @param plan_mapping The mappings accepted by the shared model planner.
#' @param model The fitted model retained for coefficient annotations.
#'
#' @return A function with the formals `layer_factory()` expects. It must name
#'   `geom`, `stat`, `position` and `params`: a `...`-only shim is stripped of
#'   all four by `create_formals()` and fails with a missing geom.
#'
#' @noRd
model_layer_fun <- function(plan_params, tag, orientation, plan_mapping, model = NULL) {
  force(plan_params)
  force(tag)
  force(orientation)
  force(plan_mapping)
  force(model)
  function(geom, stat, position, params = NULL, mapping = NULL, data = NULL, ...) {
    model_layer(
      geom = geom, stat = stat, position = position,
      mapping = plan_mapping, data = data, params = plan_params,
      orientation = orientation, tag = tag, prepared = TRUE, model = model,
      fn = "gf_model", ...
    )
  }
}
