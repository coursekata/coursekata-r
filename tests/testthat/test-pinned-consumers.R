#' Run `expr`, capturing every condition it raises (and its return value)
#'
#' A plain `tryCatch()` swallows the message that reveals a leak before it can
#' be inspected, and a plain call lets a warning print past testthat instead of
#' being counted. This runs the expression once, catching an error before it
#' unwinds and muffling anything that would otherwise print, and reports back
#' both what came out and everything that was said along the way.
#'
#' @param expr An expression to evaluate.
#'
#' @return A list with `value` (`NULL` on error) and `conditions` (character
#'   vector of every warning, message and error text raised).
capture_conditions <- function(expr) {
  conditions <- character(0)
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) {
      conditions <<- c(conditions, conditionMessage(e))
      NULL
    }),
    warning = function(w) {
      conditions <<- c(conditions, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      conditions <<- c(conditions, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, conditions = conditions)
}

test_that("every consumer survives a pinned plot and none of them says .coursekata_pin_", {
  # MUTATION: routing labels through the pinned quosures, which is what makes a
  # refusal print an internal column name at a reader. Every call below
  # is expected to succeed; a mutation that makes one of them abort on an
  # internal name would both add ".coursekata_pin_" text to `all_texts` and
  # turn one of the `expect_s3_class()` checks below into a failure on NULL.

  # a two-axis plot with a deterministic, non-symbol x -- log(Height) is
  # pinned, Thumb is a bare symbol and is left alone -- fed to every consumer
  # that needs both an x and a y and a model to measure against
  xy_plot <- gf_point(Thumb ~ log(Height), data = Fingers)
  xy_pinned <- pin_plot_values(xy_plot)$plot
  m <- lm(Thumb ~ log(Height), data = Fingers)

  # a one-axis plot with a pinned x -- show_mean(), show_cutoffs() and
  # gf_squareplot() all refuse a plot that maps a y, so they get their own plot
  x_plot <- gf_histogram(
    ~ log(Thumb), data = Fingers, binwidth = .05, fill = ~ middle(log(Thumb), .95)
  )
  x_pinned <- pin_plot_values(x_plot)$plot

  expect_true("x" %in% names(plot_pins(xy_pinned)))
  expect_true("x" %in% names(plot_pins(x_pinned)))

  results <- list(
    gf_model = capture_conditions(gf_model(xy_pinned, m)),
    gf_resid = capture_conditions(gf_model(xy_pinned, m) %>% gf_resid(m)),
    gf_square_resid = capture_conditions(gf_model(xy_pinned, m) %>% gf_square_resid(m)),
    gf_reduce = capture_conditions(gf_model(xy_pinned, m) %>% gf_reduce(m)),
    gf_square_reduce = capture_conditions(gf_model(xy_pinned, m) %>%
      gf_square_reduce(m)),
    gf_sd_ruler = capture_conditions(gf_sd_ruler(xy_pinned)),
    show_mean = capture_conditions(show_mean(x_pinned)),
    show_cutoffs = capture_conditions(show_cutoffs(x_pinned)),
    gf_squareplot = capture_conditions(gf_squareplot(x_pinned))
  )

  all_texts <- unlist(lapply(results, `[[`, "conditions"))
  expect_no_match(all_texts, ".coursekata_pin_", fixed = TRUE)
  # the categorical model's own internal outcome name must not reach a reader
  # either
  expect_no_match(all_texts, ".model_outcome", fixed = TRUE)

  for (name in names(results)) {
    expect_true(inherits(results[[name]]$value, "ggplot"), label = paste(name, "value"))
  }
})

test_that("an explicit gf_model() still works on a pinned plot", {
  # MUTATION: `plot_level` reading the plot's raw mapping instead of
  # `spec$labels` -- this is the exact call that used to make gf_model() abort
  # "`Thumb` is mapped by a layer rather than by the plot" for an outcome
  # pinned under `shuffle()`
  set.seed(1)
  base <- gf_jitter(shuffle(Thumb) ~ Height, data = Fingers)
  pinned <- pin_plot_values(base)$plot
  m <- lm(Thumb ~ Height, data = Fingers)

  q <- gf_model(pinned, m)
  expect_s3_class(q, "ggplot")

  built <- ggplot2::ggplot_build(q)
  line <- built$data[[layer_index(q, "model")]]
  predicted <- predict(m, newdata = data.frame(Height = line$x))
  expect_equal(line$y, unname(predicted))
})

test_that("an overlay on a pinned plot marks the value the plot drew", {
  # The pin exists so a later layer reads the drawn values. Checking only that a
  # ggplot came back, and that no internal name leaked, leaves the number itself
  # unguarded -- shifting the drawn mean by any amount passed both of those.
  #
  # log() is deterministic, so the expected value is computed here from the data
  # rather than by calling the code under test.
  x_plot <- gf_histogram(~ log(Thumb), data = Fingers, binwidth = .05)
  x_pinned <- pin_plot_values(x_plot)$plot

  built <- ggplot2::ggplot_build(show_mean(x_pinned))
  is_rule <- vapply(built$data, function(d) "xintercept" %in% names(d), logical(1))
  marked <- built$data[[which(is_rule)]]$xintercept

  expect_equal(marked, mean(log(Fingers$Thumb)))
})
