# The formula rung ------------------------------------------------------------------------------

# R 4.5 added datasets::penguins, whose columns are named differently. Name the package's own
# copy so this file cannot depend on the search order a parallel worker happens to leave behind.
penguins <- coursekata::penguins

# the model layer is the last one; compare it whole rather than a summary of it
model_layer <- function(p) {
  b <- ggplot2::ggplot_build(p)
  d <- b$data[[length(b$data)]]
  list(geom = class(p$layers[[length(p$layers)]]$geom)[[1]], data = d[order(names(d))])
}

# The panel range is computed from EVERY layer, so a model drawn far from the data drags the
# panel out to meet it and an "is it in the panel" check passes. Compare against the extent of
# the data layer instead.
expect_over_data <- function(p) {
  b <- ggplot2::ggplot_build(p)
  span <- function(d, nms) range(unlist(d[intersect(names(d), nms)]), na.rm = TRUE)
  data_x <- span(b$data[[1]], c("x", "xmin", "xmax"))
  data_y <- span(b$data[[1]], c("y", "ymin", "ymax"))
  d <- b$data[[length(b$data)]]
  pos <- function(nms) unlist(d[intersect(names(d), nms)], use.names = FALSE)
  xs <- pos(c("x", "xend", "xmin", "xmax", "xintercept"))
  ys <- pos(c("y", "yend", "ymin", "ymax", "yintercept"))

  expect_false(anyNA(c(xs, ys)))
  if (length(xs)) expect_true(all(xs >= data_x[[1]] - 1e-9 & xs <= data_x[[2]] + 1e-9))
  if (length(ys)) expect_true(all(ys >= data_y[[1]] - 1e-9 & ys <= data_y[[2]] + 1e-9))
  invisible(list(x = xs, y = ys))
}

test_that("an empty formula draws what lm(y ~ NULL) draws", {
  p <- gf_histogram(~body_mass_kg, data = penguins, binwidth = 0.25)
  expect_equal(
    model_layer(gf_model(p, body_mass_kg ~ NULL)),
    model_layer(gf_model(p, lm(body_mass_kg ~ NULL, data = penguins)))
  )
})

test_that("a group formula draws what lm(y ~ group) draws", {
  # boxplot, not jitter: jitter re-randomises on every build, so two builds of the same plot
  # disagree with nothing wrong. gf_boxplot() is deterministic and takes the same branch.
  p <- gf_boxplot(body_mass_kg ~ species, data = penguins)
  expect_equal(
    model_layer(gf_model(p, body_mass_kg ~ species)),
    model_layer(gf_model(p, lm(body_mass_kg ~ species, data = penguins)))
  )
})

test_that("an ANCOVA formula draws what the fitted ANCOVA draws", {
  p <- gf_point(body_mass_kg ~ flipper_length_m, color = ~species, data = penguins)
  expect_equal(
    model_layer(gf_model(p, body_mass_kg ~ species + flipper_length_m)),
    model_layer(gf_model(p, lm(body_mass_kg ~ species + flipper_length_m, data = penguins)))
  )
})

test_that("a formula draws what the fitted model draws when the outcome is on x", {
  # y ~ x describes the model, never the plot: the outcome may sit on either axis
  p <- gf_point(flipper_length_m ~ body_mass_kg, data = penguins)
  expect_equal(
    model_layer(gf_model(p, body_mass_kg ~ flipper_length_m)),
    model_layer(gf_model(p, lm(body_mass_kg ~ flipper_length_m, data = penguins)))
  )
  expect_equal(
    model_layer(gf_model(p, body_mass_kg ~ NULL)),
    model_layer(gf_model(p, lm(body_mass_kg ~ NULL, data = penguins)))
  )
})

test_that("a group formula draws what the fitted model draws when the groups are on y", {
  p <- gf_boxplot(species ~ body_mass_kg, data = penguins)
  expect_equal(
    model_layer(gf_model(p, body_mass_kg ~ species)),
    model_layer(gf_model(p, lm(body_mass_kg ~ species, data = penguins)))
  )
})

# What the formula is fit against ----------------------------------------------------------------

test_that("a formula is fit against the data the plot was built from", {
  p <- gf_point(body_mass_kg ~ flipper_length_m, data = penguins)
  d <- expect_over_data(gf_model(p, body_mass_kg ~ flipper_length_m))
  coefs <- coef(lm(body_mass_kg ~ flipper_length_m, data = penguins))
  expect_equal(range(d$y), unname(coefs[[1]] + coefs[[2]] * range(d$x)))
})

test_that("an empty formula predicts the mean of the plot's data", {
  p <- gf_point(flipper_length_m ~ body_mass_kg, data = penguins)
  d <- expect_over_data(gf_model(p, body_mass_kg ~ NULL))
  expect_equal(unique(d$x), mean(penguins$body_mass_kg))
})

test_that("a group formula predicts each group's own mean", {
  p <- gf_boxplot(species ~ body_mass_kg, data = penguins)
  d <- expect_over_data(gf_model(p, body_mass_kg ~ species))
  expect_setequal(
    round(unique(d$x), 8),
    round(unname(tapply(penguins$body_mass_kg, penguins$species, mean)), 8)
  )
})

test_that("a formula fit over a flipped scatter draws over the data, not beside it", {
  # the assertion that fails when the outcome aesthetic gets mapped: the prediction must
  # land on whichever axis the PLOT put the outcome on
  p <- gf_point(flipper_length_m ~ body_mass_kg, data = penguins)
  d <- expect_over_data(gf_model(p, body_mass_kg ~ flipper_length_m))
  expect_equal(range(d$y), range(penguins$flipper_length_m))
})

# What a formula may not be ----------------------------------------------------------------------

test_that("a formula naming a variable the plot does not have is refused by name", {
  # the formula door reaches the same validation the fitted-model door does
  p <- gf_point(body_mass_kg ~ flipper_length_m, data = penguins)
  expect_error(gf_model(p, body_mass_kg ~ bill_length_cm), "missing in plot: bill_length_cm")
})

test_that("a one-sided formula does not draw a model nobody described", {
  # NOT ADOPTED, deliberately. `~flipper_length_m` names predictors and no outcome, so the
  # only way to draw it is to guess the outcome off the axes -- which is gf_lm(), and is the
  # one thing gf_model() exists not to do. No message is asserted: the failure comes from
  # stats::lm() and its wording is not ours to keep stable.
  p <- gf_point(body_mass_kg ~ flipper_length_m, data = penguins)
  expect_error(gf_model(p, ~flipper_length_m))
})
