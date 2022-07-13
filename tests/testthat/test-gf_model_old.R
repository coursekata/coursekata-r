
# Main tests ----------------------------------------------------------------------------------

test_that("it requires at least a formula and data or a model", {
  expect_error(gf_model_old(), ".*supply a.*`gformula` and `data`.*")
  expect_error(gf_model_old(gformula = mpg ~ NULL), ".*`data`.*")
  expect_error(gf_model_old(data = mtcars), ".*`gformula`.*")
})

test_that("it wraps gf_lm() for regression models", {
  vdiffr::expect_doppelganger(
    "regression--data-formula",
    gf_model_old(gformula = mpg ~ hp, data = mtcars)
  )
})

test_that("it wraps gf_hline() to add the empty model as a line", {
  vdiffr::expect_doppelganger(
    "empty--data-formula",
    gf_model_old(gformula = mpg ~ NULL, data = mtcars)
  )
})

test_that("it draws gf_segment()s to add the group model", {
  vdiffr::expect_doppelganger(
    "group--data-formula",
    gf_model_old(gformula = Sepal.Length ~ Species, data = iris)
  )
})

test_that("it accepts a fitted model in place of a formula and data", {
  vdiffr::expect_doppelganger(
    "regression--lm",
    gf_model_old(model = lm(mpg ~ hp, data = mtcars))
  )
  vdiffr::expect_doppelganger(
    "empty--lm",
    gf_model_old(model = lm(mpg ~ NULL, data = mtcars))
  )
  vdiffr::expect_doppelganger(
    "group--lm",
    gf_model_old(model = lm(Sepal.Length ~ Species, data = iris))
  )
})

test_that("using a model supsersedes the formula and data arguments", {
  expect_warning(gf_model_old(model = lm(mpg ~ hp, data = mtcars), gformula = mpg ~ NULL))
  vdiffr::expect_doppelganger(
    "regression--model-supersedes-formula",
    suppressWarnings(gf_model_old(model = lm(mpg ~ hp, data = mtcars), gformula = mpg ~ NULL))
  )

  expect_warning(gf_model_old(model = lm(mpg ~ hp, data = mtcars), data = iris))
  vdiffr::expect_doppelganger(
    "regression--model-supersedes-data",
    suppressWarnings(gf_model_old(model = lm(mpg ~ hp, data = mtcars), data = iris))
  )
})

test_that("it allows data to be the first argument", {
  vdiffr::expect_doppelganger(
    "regression--data-first",
    gf_model_old(mtcars, mpg ~ hp)
  )
})

test_that("it allows model to be the first argument", {
  vdiffr::expect_doppelganger(
    "regression--lm-first",
    gf_model_old(lm(mpg ~ hp, data = mtcars))
  )
})

test_that("it allows formula to be the first argument", {
  vdiffr::expect_doppelganger(
    "regression--formula-first",
    gf_model_old(mpg ~ hp, data = mtcars)
  )
})


# Chaining ------------------------------------------------------------------------------------

test_that("it uses its own model or formula if chained", {
  vdiffr::expect_doppelganger(
    "regression--chained-own-model",
    gf_point(mpg ~ hp, data = mtcars) %>%
      gf_model_old(model = lm(mpg ~ hp, data = mtcars))
  )

  vdiffr::expect_doppelganger(
    "regression--chained-own-formula",
    gf_point(mpg ~ hp, data = mtcars) %>%
      gf_model_old(mpg ~ NULL)
  )
})

test_that("an informative error is given if new data is passed to it", {
  expect_error(
    gf_point(mpg ~ hp, data = mtcars) %>%
      gf_model_old(Thumb ~ Sex, data = Fingers),
    ".*different data sets.*"
  )
})

test_that("it doesn't complain about new data if the original plot had no data", {
  expect_error(gf_hline(yintercept = ~5) %>% gf_model_old(mpg ~ hp, data = mtcars), NA)
})

test_that("model can be first argument when chaining", {
  vdiffr::expect_doppelganger(
    "regression--chained--formula-in-data-arg",
    gf_point(mpg ~ hp, data = mtcars) %>%
      gf_model_old(mpg ~ NULL, data = mtcars)
  )
})

test_that("it will use the formula and/or data from earlier in the chain if it needs to", {
  vdiffr::expect_doppelganger(
    "regression--chained--inherited-data-formula",
    gf_point(mpg ~ hp, data = mtcars) %>%
      gf_model_old()
  )

  vdiffr::expect_doppelganger(
    "regression--chained--inherited-data",
    gf_point(mpg ~ hp, data = mtcars) %>%
      gf_model_old(mpg ~ NULL)
  )
})

test_that("it can chain to itself", {
  vdiffr::expect_doppelganger(
    "regression--chained--empty-and-regression",
    gf_model_old(lm(mpg ~ hp, data = mtcars)) %>%
      gf_model_old(lm(mpg ~ NULL, data = mtcars))
  )
})

test_that("it respects ... options from up the chain", {
  vdiffr::expect_doppelganger(
    "color-through-chain",
    gf_point(Thumb ~ RaceEthnic, data = Fingers, color = ~RaceEthnic) %>%
      gf_model_old()
  )
})

test_that("it can modify ... options from up the chain", {
  vdiffr::expect_doppelganger(
    "override-color-from-chain",
    gf_point(Thumb ~ RaceEthnic, data = Fingers, color = ~RaceEthnic) %>%
      gf_model_old(color = ~"red")
  )
})


# Histograms ----------------------------------------------------------------------------------

test_that("it draws gf_vline()s on faceted histograms", {
  vdiffr::expect_doppelganger(
    "histogram-wrap",
    gf_histogram(~mpg, data = mtcars) %>%
      gf_facet_wrap(~cyl) %>%
      gf_model_old()
  )

  vdiffr::expect_doppelganger(
    "histogram-grid",
    gf_histogram(~mpg, data = mtcars) %>%
      gf_facet_grid(cyl ~ .) %>%
      gf_model_old()
  )

  vdiffr::expect_doppelganger(
    "dhistogram",
    gf_dhistogram(~mpg, data = mtcars) %>%
      gf_facet_wrap(~cyl) %>%
      gf_model_old()
  )

  vdiffr::expect_doppelganger(
    "histogram-null-specified",
    gf_histogram(~mpg, data = mtcars) %>%
      gf_facet_wrap(~cyl) %>%
      gf_model_old(mpg ~ NULL)
  )

  vdiffr::expect_doppelganger(
    "histogram-group-specified",
    gf_histogram(~mpg, data = mtcars) %>%
      gf_facet_wrap(~cyl) %>%
      gf_model_old(mpg ~ cyl)
  )

  vdiffr::expect_doppelganger(
    "histogram-group-inferred",
    gf_histogram(~mpg, data = mtcars) %>%
      gf_facet_wrap(~cyl) %>%
      gf_model_old(mpg ~ cyl)
  )
})

test_that("it draws the empty model on non-faceted histograms", {
  vdiffr::expect_doppelganger(
    "histogram-empty-inferred",
    gf_histogram(~mpg, data = mtcars) %>%
      gf_model_old()
  )
})

test_that("it works with rotated histograms", {
  vdiffr::expect_doppelganger(
    "histogram-rotated",
    gf_histogramh(~mpg, data = mtcars) %>%
      gf_facet_wrap(~cyl) %>%
      gf_model_old(mpg ~ cyl)
  )
})

test_that("it works with other single variable models", {
  vdiffr::expect_doppelganger(
    "density",
    gf_density(~mpg, data = mtcars) %>%
      gf_facet_wrap(~cyl) %>%
      gf_model_old(mpg ~ cyl)
  )

  vdiffr::expect_doppelganger(
    "dot",
    gf_dotplot(~mpg, data = mtcars, binwidth = 1) %>%
      gf_facet_wrap(~cyl) %>%
      gf_model_old(mpg ~ cyl)
  )
})

# test_that("it will fail if it can't determine the formula based on the plot and facets", {
#   expect_error(
#     gf_histogram(~mpg, data = mtcars) %>%
#       gf_facet_grid(am ~ cyl) %>%
#       gf_model_old(),
#     ".*be more specific.*"
#   )
# })
#
# # test_that("it throws a warning when trying to chain to something it can't work with", {
# #   # taking a white list approach, so only testing one unsupported here which will error by default,
# #   # and then all explicitly supported below with no error
# #
# #   # unsupported
# #   expect_warning(gf_density_2d(eruptions ~ waiting, data = faithful) %>% gf_model_old())
# #
# #   # supported
# #   expect_warning(gf_point(mpg ~ hp, data = mtcars) %>% gf_model_old(), NA)
# #   expect_warning(gf_lm(mpg ~ hp, data = mtcars) %>% gf_model_old(), NA)
# #   expect_warning(gf_hline(yintercept = ~5) %>% gf_model_old(mpg ~ NULL, data = mtcars), NA)
# #   expect_warning(gf_vline(xintercept = ~5) %>% gf_model_old(mpg ~ NULL, data = mtcars), NA)
# #   expect_warning(gf_abline(slope = ~3, intercept = ~2) %>% gf_model_old(mpg ~ NULL, data = mtcars), NA)
# #   expect_warning(gf_histogram(~mpg, data = mtcars) %>% gf_model_old(), NA)
# #   # expect_warning(gf_density(~mpg, data = mtcars) %>% gf_model_old(), NA)
# #   # expect_warning(gf_boxplot(~mpg, data = mtcars) %>% gf_model_old(), NA)
# #   # expect_warning(gf_violin(~mpg, data = mtcars) %>% gf_model_old(), NA)
# # })
#
#
# # Alternate formula specification -------------------------------------------------------------
#
# test_that("it allows the fitted model to use data$var syntax in the formula", {
#   vdiffr::expect_doppelganger(
#     'regression--lm-with-data$var',
#     gf_model_old(lm(mtcars$mpg ~ mtcars$hp))
#   )
#   vdiffr::expect_doppelganger(
#     'empty--lm-with-data$var',
#     gf_model_old(lm(mtcars$mpg ~ NULL))
#   )
#   vdiffr::expect_doppelganger(
#     'group--lm-with-data$var',
#     gf_model_old(lm(iris$Sepal.Length ~ iris$Species))
#   )
# })
#
# test_that("it allows the formula to use data$var syntax, if data can be found", {
#   skip("Planned improvement")
# })
