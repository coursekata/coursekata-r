test_that("extracted values are correct", {
  test_model <- lm(mpg ~ hp, data = mtcars)

  expect_equal(b0(test_model), test_model$coefficients[[1]])
  expect_equal(b1(test_model), coefficients(test_model)[[2]])
  expect_equal(pre(test_model), summary(test_model)$r.squared)

  f_full_expected <- summary(test_model)$fstatistic
  expect_equal(f(test_model), f_full_expected[["value"]])
  expect_equal(p(test_model), pf(
    f_full_expected[["value"]],
    f_full_expected[["numdf"]],
    f_full_expected[["dendf"]],
    lower.tail = FALSE
  ))
})

test_that("values can be extracted from fitted lm or formula-and-data", {
  estimate_funs <- c(b0, b1, b, f, pre, p)
  purrr::iwalk(estimate_funs, ~ expect_identical(.x(mpg ~ hp, mtcars), .x(lm(mpg ~ hp, mtcars))))
})

test_that("they can extract all related terms (not just full model terms)", {
  mult_model <- lm(mpg ~ hp * cyl, data = mtcars)
  sup_out <- supernova(mult_model, type = 3)

  expect_equal(b(mult_model, all = TRUE), list(
    "b_0" = coefficients(mult_model)[[1]],
    "b_hp" = coefficients(mult_model)[["hp"]],
    "b_cyl" = coefficients(mult_model)[["cyl"]],
    "b_hp:cyl" = coefficients(mult_model)[["hp:cyl"]]
  ))

  expect_equal(f(mult_model, all = TRUE, type = 3), list(
    "f" = sup_out$tbl$F[[1]],
    "f_hp" = sup_out$tbl$F[[2]],
    "f_cyl" = sup_out$tbl$F[[3]],
    "f_hp:cyl" = sup_out$tbl$F[[4]]
  ))

  expect_equal(pre(mult_model, all = TRUE, type = 3), list(
    "pre" = sup_out$tbl$PRE[[1]],
    "pre_hp" = sup_out$tbl$PRE[[2]],
    "pre_cyl" = sup_out$tbl$PRE[[3]],
    "pre_hp:cyl" = sup_out$tbl$PRE[[4]]
  ))

  expect_equal(p(mult_model, all = TRUE, type = 3), list(
    "p" = sup_out$tbl$p[[1]],
    "p_hp" = sup_out$tbl$p[[2]],
    "p_cyl" = sup_out$tbl$p[[3]],
    "p_hp:cyl" = sup_out$tbl$p[[4]]
  ))
})

test_that("it throws useful error messages when used with empty model inappropriately", {
  empty_model <- lm(mpg ~ NULL, data = mtcars)
  error_pattern <- ".*[Cc]an't.*empty model.*"
  expect_error(f(empty_model), error_pattern)
  expect_error(pre(empty_model), error_pattern)
  expect_error(p(empty_model), error_pattern)
})

test_that("they return a scalar if a single term is requested", {
  mult_model <- lm(mpg ~ hp * cyl, data = mtcars)
  terms <- c("hp")
  named <- function(x) paste(x, terms, sep = "_")

  expect_equal(b(mult_model, predictor = terms), b(mult_model, all = TRUE)[[named("b")]])
  expect_equal(f(mult_model, predictor = terms), f(mult_model, all = TRUE)[[named("f")]])
  expect_equal(pre(mult_model, predictor = terms), pre(mult_model, all = TRUE)[[named("pre")]])
  expect_equal(p(mult_model, predictor = terms), p(mult_model, all = TRUE)[[named("p")]])
})

test_that("they return a named list of the requested terms if 2+ terms are requested", {
  mult_model <- lm(mpg ~ hp * cyl, data = mtcars)
  terms <- c("hp", "hp:cyl")
  named <- function(x) paste(x, terms, sep = "_")

  expect_equal(b(mult_model, predictor = terms), b(mult_model, all = TRUE)[named("b")])
  expect_equal(f(mult_model, predictor = terms), f(mult_model, all = TRUE)[named("f")])
  expect_equal(pre(mult_model, predictor = terms), pre(mult_model, all = TRUE)[named("pre")])
  expect_equal(p(mult_model, predictor = terms), p(mult_model, all = TRUE)[named("p")])
})

test_that("term filtering works with formulae", {
  mult_model <- lm(mpg ~ hp * cyl, data = mtcars)
  terms <- c("hp", "hp:cyl")
  frms <- purrr::map(terms, asOneSidedFormula)
  named <- function(x) paste(x, terms, sep = "_")

  expect_equal(b(mult_model, predictor = frms), b(mult_model, all = TRUE)[named("b")])
  expect_equal(f(mult_model, predictor = frms), f(mult_model, all = TRUE)[named("f")])
  expect_equal(pre(mult_model, predictor = frms), pre(mult_model, all = TRUE)[named("pre")])
  expect_equal(p(mult_model, predictor = frms), p(mult_model, all = TRUE)[named("p")])
})

test_that("using data with missing values doesn't result in mutliple refitting messages", {
  data_missing <- mtcars
  data_missing[1, "hp"] <- NA
  expect_message(
    f(mpg ~ hp, data = data_missing),
    "(?!Refitting.*Refitting)Refitting",
    perl = TRUE
  )
})
