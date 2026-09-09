mean_argument_harness <- function(object = NULL, plot = lifecycle::deprecated()) {
  normalize_plot_argument(
    object, plot, missing(object), missing(plot), "show_mean"
  )
}

labels_argument_harness <- function(show_labels = FALSE,
                                    labels = lifecycle::deprecated()) {
  normalize_show_labels_argument(
    show_labels, labels, missing(show_labels), missing(labels)
  )
}

cutoff_argument_harness <- function(object = NULL, part,
                                    plot = lifecycle::deprecated()) {
  object_missing <- missing(object)
  part_missing <- missing(part)
  plot_missing <- missing(plot)
  shape <- normalize_cutoff_call_shape(
    enquo(object), enquo(part), object_missing, part_missing, plot_missing,
    user_call = sys.call()
  )

  object_value <- if (shape$object_missing) NULL else eval_tidy(shape$object)
  object_value <- normalize_plot_argument(
    object_value, plot, shape$object_missing, plot_missing, "show_cutoffs"
  )

  list(
    object = object_value,
    part = shape$part,
    part_missing = shape$part_missing,
    legacy_plot_part = shape$legacy_plot_part
  )
}

test_that("canonical object is returned and deprecated plot is translated", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg))
  signals <- list()
  local_mocked_bindings(
    deprecate_warn = function(when, what, with = NULL, ...) {
      signals[[length(signals) + 1L]] <<- list(when = when, what = what, with = with)
      invisible(NULL)
    },
    .package = "lifecycle"
  )

  expect_identical(mean_argument_harness(p), p)
  expect_length(signals, 0)

  expect_identical(mean_argument_harness(plot = p), p)
  expect_equal(
    signals,
    list(list(when = "0.21.0", what = "show_mean(plot)",
              with = "show_mean(object)"))
  )
})

test_that("canonical object and deprecated plot conflict", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg))
  expect_error(
    mean_argument_harness(object = p, plot = p),
    "both `object` and deprecated `plot`"
  )
})

test_that("public distribution helpers warn once for plot and reject conflicts", {
  withr::local_options(lifecycle_verbosity = "default")
  values <- data.frame(x = c(-2, -1, 1, 2))
  p <- ggplot2::ggplot(values, ggplot2::aes(x)) +
    ggplot2::geom_histogram(bins = 4)
  cache <- get("deprecation_env", asNamespace("lifecycle"))
  ids <- c("coursekata-show_mean-plot", "coursekata-show_dgp-plot")
  clear <- function() {
    present <- ids[vapply(ids, exists, logical(1), envir = cache, inherits = FALSE)]
    if (length(present) > 0L) rlang::env_unbind(cache, present)
  }
  clear()
  withr::defer(clear())

  warnings <- character()
  withCallingHandlers(
    {
      mean_plot <- show_mean(plot = p)
      show_mean(plot = p)
      dgp_plot <- show_dgp(plot = p)
      show_dgp(plot = p)
    },
    lifecycle_warning_deprecated = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 2)
  expect_match(warnings[[1]], "The `plot` argument of `show_mean\\(\\)` is deprecated")
  expect_match(warnings[[2]], "The `plot` argument of `show_dgp\\(\\)` is deprecated")
  expect_true(all(grepl("Please use the `object` argument instead", warnings)))
  expect_s3_class(mean_plot, "ggplot")
  expect_s3_class(dgp_plot, "ggplot")

  expect_error(
    show_mean(object = p, plot = p),
    "both `object` and deprecated `plot`"
  )
  expect_error(
    show_dgp(object = p, plot = p),
    "both `object` and deprecated `plot`"
  )
})

test_that("canonical show_labels is returned and deprecated labels is translated", {
  signals <- list()
  local_mocked_bindings(
    deprecate_warn = function(when, what, with = NULL, ...) {
      signals[[length(signals) + 1L]] <<- list(when = when, what = what, with = with)
      invisible(NULL)
    },
    .package = "lifecycle"
  )

  expect_false(labels_argument_harness())
  expect_true(labels_argument_harness(show_labels = TRUE))
  expect_length(signals, 0)

  expect_true(labels_argument_harness(labels = TRUE))
  expect_equal(
    signals,
    list(list(when = "0.21.0", what = "show_cutoffs(labels)",
              with = "show_cutoffs(show_labels)"))
  )
})

test_that("canonical show_labels and deprecated labels conflict", {
  expect_error(
    labels_argument_harness(show_labels = FALSE, labels = TRUE),
    "both `show_labels` and deprecated `labels`"
  )
})

test_that("named legacy plot plus positional cutoff part is preserved", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg))
  signals <- list()
  local_mocked_bindings(
    deprecate_warn = function(when, what, with = NULL, ...) {
      signals[[length(signals) + 1L]] <<- list(when = when, what = what, with = with)
      invisible(NULL)
    },
    .package = "lifecycle"
  )

  result <- cutoff_argument_harness(plot = p, middle(mpg, 0.95))

  expect_identical(result$object, p)
  expect_equal(as_label(result$part), "middle(mpg, 0.95)")
  expect_false(result$part_missing)
  expect_true(result$legacy_plot_part)
  expect_equal(
    signals,
    list(list(when = "0.21.0", what = "show_cutoffs(plot)",
              with = "show_cutoffs(object)"))
  )
})

test_that("qualified legacy cutoff parts are preserved", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg))
  result <- suppressWarnings(
    cutoff_argument_harness(plot = p, coursekata::middle(mpg, 0.95))
  )

  expect_identical(result$object, p)
  expect_equal(as_label(result$part), "coursekata::middle(mpg, 0.95)")
  expect_true(result$legacy_plot_part)
})

test_that("real object and plot conflicts are not mistaken for a legacy part", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg))

  expect_error(
    cutoff_argument_harness(object = p, plot = p),
    "both `object` and deprecated `plot`"
  )
  expect_error(
    cutoff_argument_harness(p, plot = p),
    "both `object` and deprecated `plot`"
  )
})

test_that("ordinary canonical cutoff calls keep their argument roles", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg))
  result <- cutoff_argument_harness(p, middle(mpg, 0.95))

  expect_identical(result$object, p)
  expect_equal(as_label(result$part), "middle(mpg, 0.95)")
  expect_false(result$part_missing)
  expect_false(result$legacy_plot_part)
})
