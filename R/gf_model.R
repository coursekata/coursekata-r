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
#' @param model A linear model fit by either [`lm()`] or [`aov()`].
#' @param ... Additional arguments. Typically these are (a) ggplot2 aesthetics to be set with
#'   `attribute = value`, (b) ggplot2 aesthetics to be mapped with `attribute = ~ expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'
#' @return a gg object (a plot layer) that can be added to a plot.
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
gf_model <- function(object, model, ...) {
  if (!inherits(object, c("gg", "ggplot"))) {
    abort("`gf_model()` needs to be layered on top of a plot.")
  }

  spec <- plot_spec(object)
  plan <- model_plan(spec, model_spec(spec$data, model), list2(...))

  render_model_plan(plan, object)
}

#' Turn a model plan into a layer
#'
#' @param plan A [model_plan()] list.
#' @param object The plot the layer is being added to.
#'
#' @return The plot, with the model layer added and tagged.
#'
#' @noRd
render_model_plan <- function(plan, object) {
  plotter <- switch(plan$kind,
    hline = ggformula::gf_hline,
    vline = ggformula::gf_vline,
    line = ggformula::gf_line,
    errorbar = ggformula::gf_errorbar
  )

  args <- plan$args
  args$object <- object
  args$data <- plan$grid

  built <- do.call(plotter, args)
  n <- length(built$layers)
  built$layers[[n]] <- tag_layer(built$layers[[n]], plan$tag)
  built
}
