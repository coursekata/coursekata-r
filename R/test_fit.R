#' Split data into train and test sets.
#'
#' @param data A data frame.
#' @param prop The proportion of rows to assign to the training set.
#'
#' @return A list with two data frames, `train` and `test`.
#'
#' @export
#' @examples
#' set.seed(123)
#' parts <- split_data(Fingers, prop = 0.8)
#' nrow(parts$train)
#' nrow(parts$test)
split_data <- function(data, prop = .7) {
  row_numbers <- seq_len(nrow(data))
  train_rows <- sample(row_numbers, floor(prop * nrow(data)))
  list(train = data[train_rows, ], test = data[-train_rows, ])
}

#' Test the fit of a model on a train and test set.
#'
#' @param model An [`lm`] model.
#' @param df_train A data frame with the training data.
#' @param df_test A data frame with the test data.
#'
#' @return A data frame with the fit statistics.
#'
#' @export
#' @examples
#' set.seed(123)
#' parts <- split_data(Fingers)
#' model <- lm(Thumb ~ Height, data = parts$train)
#' fit_stats(model, parts$train, parts$test)
#' fitstats(model, parts$train, parts$test)
fit_stats <- function(model, df_train, df_test) {
  fit_train <- stats::update(model, data = df_train)
  fit_test  <- stats::update(model, data = df_test)
  predictor <- as.character(rlang::f_lhs(stats::formula(model)))

  tibble::tibble(
    data = c("train", "test"),
    F = c(f(fit_train), f(fit_test)),
    PRE = c(pre(fit_train), pre(fit_test)),
    RMSE = c(
      Metrics::rmse(df_train[[predictor]], stats::predict(fit_train)),
      Metrics::rmse(df_test[[predictor]], stats::predict(fit_test))
    ),
  )
}

#' @rdname fit_stats
#' @export
fitstats <- fit_stats
