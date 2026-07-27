#' Add Residual Lines to a Plot
#'
#' This function adds vertical lines representing residuals from a linear model to a ggformula plot.
#' The residuals are drawn from the observed data points to the predicted values from the model.
#'
#' @param plot A ggformula plot object, typically created with `gf_point()`.
#' @param model A fitted linear model object created using `lm()`.
#' @param linewidth A numeric value specifying the width of the residual lines. Default is `0.2`.
#' @param ... Additional aesthetics passed to `geom_segment()`, such as `color`, `alpha`,
#'   `linetype`.
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
gf_resid <- function(plot, model, linewidth = 0.2, ...) {
  plot <- freeze_jitter(plot)
  geometry <- plot_geometry(plot)
  plan <- resid_plan(geometry, stats::predict(model))
  render_resid_plan(plot, plan, linewidth, ...)
}

#' Add Squared Residual Visualization to a Plot
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function adds squared residual representations to a ggformula plot, illustrating
#' squared error as a polygon. The function dynamically adjusts the aspect ratio to ensure
#' proper scaling of squares.
#'
#' @param plot A ggformula plot object, typically created with `gf_point()`.
#' @param model A fitted linear model object created using `lm()`.
#' @param aspect A numeric value controlling the square's aspect ratio. Default is `4/6`.
#' @param alpha A numeric value specifying the transparency of the square's fill. Default is `0.1`.
#' @param ... Additional aesthetics passed to `geom_polygon()`, such as `color` and `fill`.
#'
#' @return A ggplot object with squared residuals added.
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
gf_square_resid <- function(plot, model, aspect = 4 / 6, alpha = 0.1, ...) {
  lifecycle::signal_stage("experimental", "gf_square_resid()")

  plot <- freeze_jitter(plot)
  geometry <- plot_geometry(plot)
  plan <- square_resid_plan(geometry, stats::predict(model), aspect)
  render_square_resid_plan(plot, plan, alpha, ...)
}

#' @rdname gf_square_resid
#' @description
#' `gf_squaresid()` is a fully supported alias of `gf_square_resid()`. The
#' name honors [Tyler Haslam](https://github.com/TH4SL4M), the Utah high
#' school teacher whose efforts shaped the residual and squared-residual
#' visualizations and who requested this function by that name.
#' @export
gf_squaresid <- function(plot, model, aspect = 4 / 6, alpha = 0.1, ...) {
  args <- list(plot = plot, model = model, ...)
  if (!missing(aspect)) args$aspect <- aspect
  if (!missing(alpha)) args$alpha <- alpha
  do.call(gf_square_resid, args)
}
