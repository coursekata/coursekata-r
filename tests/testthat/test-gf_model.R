
# Main tests ----------------------------------------------------------------------------------

test_that("it requires at least a formula and data or a model", {
  expect_error(gf_model(), ".*supply a.*`gformula` and `data`.*")
  expect_error(gf_model(gformula = mpg ~ NULL), ".*`data`.*")
  expect_error(gf_model(data = mtcars), ".*`gformula`.*")
})

test_that("it wraps gf_lm() for regression models", {
  vdiffr::expect_doppelganger(
    'regression--data-formula',
    gf_model(gformula = mpg ~ hp, data = mtcars)
  )
})

test_that("it wraps gf_hline() to add the empty model as a line", {
  vdiffr::expect_doppelganger(
    'empty--data-formula',
    gf_model(gformula = mpg ~ NULL, data = mtcars)
  )
})

test_that("it draws gf_segments() to add the group model", {
  vdiffr::expect_doppelganger(
    'group--data-formula',
    gf_model(gformula = Sepal.Length ~ Species, data = iris)
  )
})

test_that("it accepts a fitted model in place of a formula and data", {
  vdiffr::expect_doppelganger(
    'regression--lm',
    gf_model(model = lm(mpg ~ hp, data = mtcars))
  )
  vdiffr::expect_doppelganger(
    'empty--lm',
    gf_model(model = lm(mpg ~ NULL, data = mtcars))
  )
  vdiffr::expect_doppelganger(
    'group--lm',
    gf_model(model = lm(Sepal.Length ~ Species, data = iris))
  )
})

test_that("using a model supsersedes the formula and data arguments", {
  vdiffr::expect_doppelganger(
    'regression--model-supersedes-formula',
    gf_model(model = lm(mpg ~ hp, data = mtcars), gformula = mpg ~ NULL)
  )
  vdiffr::expect_doppelganger(
    'regression--model-supersedes-data',
    gf_model(model = lm(mpg ~ hp, data = mtcars), data = iris)
  )
})

test_that("it allows data to be the first argument", {
  vdiffr::expect_doppelganger(
    'regression--data-first',
    gf_model(mtcars, mpg ~ hp)
  )
})

test_that("it allows model to be the first argument", {
  vdiffr::expect_doppelganger(
    'regression--lm-first',
    gf_model(lm(mpg ~ hp, data = mtcars))
  )
})

test_that("it allows formula to be the first argument", {
  vdiffr::expect_doppelganger(
    'regression--formula-first',
    gf_model(mpg ~ hp, data = mtcars)
  )
})


# Chaining ------------------------------------------------------------------------------------

test_that("it uses its own model or formula if chained", {
  vdiffr::expect_doppelganger(
    'regression--chained-own-model',
    gf_point(mpg ~ hp, data = mtcars) %>%
      gf_model(model = lm(mpg ~ hp, data = mtcars))
  )

  vdiffr::expect_doppelganger(
    'regression--chained-own-formula',
    gf_point(mpg ~ hp, data = mtcars) %>%
      gf_model(mpg ~ NULL)
  )
})

test_that("an informative error is given if new data is passed to it", {
  expect_error(
    gf_point(mpg ~ hp, data = mtcars) %>%
      gf_model(Thumb ~ Sex, data = Fingers),
    ".*different data sets.*"
  )
})

test_that("model can be first argument when chaining", {
  vdiffr::expect_doppelganger(
    'regression--chained--formula-in-data-arg',
    gf_point(mpg ~ hp, data = mtcars) %>%
      gf_model(mpg ~ NULL, data = mtcars)
  )
})

test_that("it will use the formula and/or data from earlier in the chain if it needs to", {
  vdiffr::expect_doppelganger(
    "regression--chained--inherited-data-formula",
    gf_point(mpg ~ hp, data = mtcars) %>%
      gf_model()
  )

  vdiffr::expect_doppelganger(
    "regression--chained--inherited-data",
    gf_point(mpg ~ hp, data = mtcars) %>%
      gf_model(mpg ~ NULL)
  )
})

test_that("it can chain to itself", {
  vdiffr::expect_doppelganger(
    'regression--chained--empty-and-regression',
    gf_model(lm(mpg ~ hp, data = mtcars)) %>%
      gf_model(lm(mpg ~ NULL, data = mtcars))
  )
})

test_that("it respects ... options from up the chain", {
  vdiffr::expect_doppelganger(
    'color-through-chain',
    gf_point(Thumb ~ RaceEthnic, data = Fingers, color = ~RaceEthnic) %>%
      gf_model()
  )
})

test_that("it can modify ... options from up the chain", {
  vdiffr::expect_doppelganger(
    'override-color-from-chain',
    gf_point(Thumb ~ RaceEthnic, data = Fingers, color = ~RaceEthnic) %>%
      gf_model(color = ~"red")
  )
})


# Alternate formula specification -------------------------------------------------------------

test_that("it allows the fitted model to use data$var syntax in the formula", {
  vdiffr::expect_doppelganger(
    'regression--lm-with-data$var',
    gf_model(lm(mtcars$mpg ~ mtcars$hp))
  )
  vdiffr::expect_doppelganger(
    'empty--lm-with-data$var',
    gf_model(lm(mtcars$mpg ~ NULL))
  )
  vdiffr::expect_doppelganger(
    'group--lm-with-data$var',
    gf_model(lm(iris$Sepal.Length ~ iris$Species))
  )
})

test_that("it allows the formula to use data$var syntax, if data can be found", {
  skip("Planned improvement")
})
