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
#'   `model =`.
#' @param gformula Not used. `gf_model()` draws a model, not an aesthetic
#'   formula; a model given positionally lands here and is moved to `model`.
#' @param data Not used. The layer draws the model's predictions, which are
#'   computed from the data the plot was built from.
#' @param ... Additional arguments. Typically these are (a) ggplot2 aesthetics to be set with
#'   `attribute = value`, (b) ggplot2 aesthetics to be mapped with `attribute = ~ expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#' @param xlab,ylab,title,subtitle,caption Labels for the plot.
#' @param geom Not set by the caller. The geometry is derived from the model.
#' @param stat,position Reach the layer as given, but `gf_model()` already computed
#'   the model's predictions before the layer is built, so changing these
#'   recomputes something else on top of that prediction grid (`stat = "smooth"`
#'   re-smooths it, for example) rather than changing how the model's own claim is
#'   drawn. Leave them at their defaults.
#' @param show.legend Whether this layer contributes to the legend.
#' @param show.help Print the layer's own help instead of drawing.
#' @param inherit Not set by the caller. Whether the layer inherits the plot's
#'   aesthetics is derived per model shape: an intercept states its own
#'   position, everything else inherits the axis the plot put the outcome on.
#' @param environment The environment mappings are resolved in.
#'
#' @return a gg object (a plot layer) that can be added to a plot.
#'
#' @export
#' @importFrom ggformula layer_factory
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
gf_model <- ggformula::layer_factory(
  geom = "line",
  stat = "identity",
  position = "identity",
  aes_form = NULL,
  extras = alist(model = ),
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
      spec <- coursekata:::model_layer_spec(
        object,
        if (missing(model)) NULL else model,
        rlang::list2(...)
      )
      geom <- spec$geom
      data <- spec$data
      aesthetics <- spec$aesthetics
      inherit <- spec$inherit
      layer_fun <- coursekata:::model_layer_fun(spec$params, spec$tag)
    }
  }
)

#' Translate a model into the pieces a ggformula layer is built from
#'
#' THE INVARIANT: of the two positional aesthetics `x` and `y`, this never maps
#' the one the plot is using to carry the model's outcome. The plot has already
#' put the outcome on an axis; the model layer leaves that aesthetic free and
#' inherits it. That is the only reason a flipped plot draws correctly, and
#' there is no orientation logic anywhere at draw time to make it correct any
#' other way. `xend`/`yend` and the intercepts are terminal companions that
#' ggplot2 cannot inherit, so they are named outright; `x` and `y` are not.
#'
#' @param object The plot the layer is being added to.
#' @param model A model fit by `lm()` or `aov()`, or the formula for one.
#' @param args Named list of user arguments, from `...`.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with `geom`, `data`, `aesthetics`, `params`, `inherit`, `tag`.
#'
#' @noRd
model_layer_spec <- function(object, model, args = list(), call = caller_env()) {
  if (!inherits(object, c("gg", "ggplot"))) {
    abort("`gf_model()` needs to be layered on top of a plot.", call = call)
  }

  if (is.null(model)) {
    abort(
      c(
        "`gf_model()` needs to be told which model to draw",
        i = "a model you already fit: `gf_model(lm(body_mass_kg ~ species, data = penguins))`",
        i = "or the model written as a formula: `gf_model(body_mass_kg ~ species)`"
      ),
      call = call
    )
  }

  if (is_formula(model) && is.null(f_lhs(model))) {
    abort(
      c(
        "`gf_model()` needs to be told what the model predicts",
        x = glue("`{deparse1(model)}` names predictors but no outcome"),
        i = "write the outcome on the left: `body_mass_kg ~ species`",
        i = "a model with no predictors is written `body_mass_kg ~ NULL`"
      ),
      call = call
    )
  }

  spec <- plot_spec(object)
  mspec <- model_spec(spec$data, model, call = call)
  plan <- model_plan(spec, mspec, args, call = call)

  # the fit line and the group mark both leave the outcome's axis unmapped and
  # inherit it, so it has to be mapped by the plot rather than by a layer
  # underneath it; an intercept states its position outright and does not care
  inherits_outcome <- plan$kind %in% c("line", "segment")
  plot_level <- purrr::map_chr(object$mapping, as_label)
  if (inherits_outcome && mspec$outcome %in% label_columns(plot_level) == FALSE) {
    abort(
      c(
        glue("`{mspec$outcome}` is mapped by a layer rather than by the plot"),
        paste0(
          "gf_model() draws the fit line along the plot's own aesthetics, so the outcome ",
          "has to be mapped on the plot for the line to inherit it"
        ),
        i = "map it in ggplot(data, aes(...)), or build the plot with ggformula"
      ),
      call = call
    )
  }

  mapped <- purrr::map_lgl(plan$args, ~ is_formula(.x) && length(.x) == 2L)

  list(
    geom = switch(plan$kind,
      hline = ggplot2::GeomHline,
      vline = ggplot2::GeomVline,
      line = ggplot2::GeomLine,
      segment = GeomModelMark
    ),
    data = plan$grid,
    aesthetics = do.call(ggplot2::aes, purrr::map(plan$args[mapped], f_rhs)),
    params = plan$args[!mapped],
    # an intercept carries the whole claim and spans the panel on its own, so
    # the plot's x and y must not reach it; every other shape needs them
    inherit = !(plan$kind %in% c("hline", "vline")),
    tag = plan$tag
  )
}

#' Build the layer function that draws a model
#'
#' Everything the caller typed has already been through `model_plan()`, which is
#' the one place that decides what a model looks like on a plot. Taking
#' ggformula's copy of those arguments as well would supply `color` beside the
#' plan's `colour` and duplicate the aesthetic.
#'
#' @param plan_params The plan's static arguments.
#' @param tag The tag to name the layer with.
#'
#' @return A function with the formals `layer_factory()` expects. It must name
#'   `geom`, `stat`, `position` and `params`: a `...`-only shim is stripped of
#'   all three by `create_formals()` and fails with a missing geom.
#'
#' @noRd
model_layer_fun <- function(plan_params, tag) {
  force(plan_params)
  force(tag)
  function(geom, stat, position, params = NULL, mapping = NULL, data = NULL, ...) {
    tag_layer(
      ggplot2::layer(
        geom = geom, stat = stat, position = position,
        mapping = mapping, data = data, params = plan_params, ...
      ),
      tag
    )
  }
}
