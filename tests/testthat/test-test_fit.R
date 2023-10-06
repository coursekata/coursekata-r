test_that("it splits data proportionately", {
  data <- tibble::tibble(x = 1:100)
  split <- split_data(data, .7)
  expect_equal(nrow(split$train), 70)
  expect_equal(nrow(split$test), 30)
})

test_that("it splits data randomly", {
  data <- tibble::tibble(x = 1:1000)
  split1 <- split_data(data, .7)
  split2 <- split_data(data, .7)
  expect_false(all(split1$train == split2$train))
  expect_false(all(split1$test == split2$test))
})

test_that("it compares the fit of a model on train and test data", {
  model <- lm(mpg ~ cyl, data = mtcars)
  split <- split_data(mtcars, .7)
  fit_train <- lm(model, data = split$train)
  fit_test <- lm(model, data = split$test)
  stats <- fit_stats(model, split$train, split$test)

  expect_equal(stats$data, c("train", "test"))
  expect_equal(stats$F, c(f(fit_train), f(fit_test)))
  expect_equal(stats$PRE, c(pre(fit_train), pre(fit_test)))
  expect_equal(stats$RMSE, c(
    rmse(split$train$mpg, predict(fit_train)),
    rmse(split$test$mpg, predict(fit_test))
  ))
})
