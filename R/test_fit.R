#' Split data into train and test sets.
#'
#' @param data A data frame.
#' @param prop The proportion of rows to assign to the training set.
#' @return A list with two data frames, `train` and `test`.
#' @export
split_data <- function(data, prop = .7) {
  row_numbers <- seq_len(nrow(data))
  train_rows <- sample(row_numbers, floor(prop * nrow(data)))
  list(train = data[train_rows, ], test = data[-train_rows, ])
}

#' Test the fit of a model on a train and test set.
#'
#' @param model An [`lm`] model (or anything compatible with [`formula()`]).
#' @param df_train A data frame with the training data.
#' @param df_test A data frame with the test data.
#' @return A data frame with the fit statistics.
#' @export
test_fit <- function(model, df_train, df_test) {
  fit_train <- stats::update(model, data = df_train)
  fit_test  <- stats::update(model, data = df_test)
  predictor <- m_predictor(model)

  tibble::tibble(
    data = c(
      'train',
      'test'
    ),
    F = c(
      coursekata::f(fit_train),
      coursekata::f(fit_test)
    ),
    PRE = c(
      coursekata::pre(fit_train),
      coursekata::pre(fit_test)
    ),
    RMSE = c(
      Metrics::rmse(df_train[[predictor]], stats::predict(fit_train)),
      Metrics::rmse(df_test[[predictor]], stats::predict(fit_test))
    ),
  )
}

#' Get the predictor variable from a model.
#'
#' @param model An [`lm`] model (or anything compatible with [`formula()`]).
#' @return A string with the name of the predictor variable.
#' @keywords internal
m_predictor <- function(model) {
  as.character(rlang::f_lhs(stats::formula(model)))
}

