spec_for <- function(frm) {
  p <- gf_histogram(~Thumb, data = Fingers, binwidth = 5, fill = frm)
  cutoff_spec(plot_spec(p)$resolve_aes("fill"))
}

test_that("the proportion is read by name, not by position", {
  expect_equal(spec_for(~ middle(Thumb, .90))$prop, .90)
  expect_equal(spec_for(~ middle(Thumb, prop = .90))$prop, .90)
  expect_equal(spec_for(~ middle(prop = .90, x = Thumb))$prop, .90)
})

test_that("a fill relying on the documented default is accepted", {
  expect_equal(spec_for(~ middle(Thumb))$prop, .95)
  expect_equal(spec_for(~ tails(Thumb))$prop, .95)
  expect_equal(spec_for(~ upper(Thumb))$prop, .025)
})

test_that("a namespace-qualified call resolves to the function it names", {
  expect_equal(spec_for(~ coursekata::middle(Thumb, .95))$func, "middle")
})

test_that("greedy is read from the fill call", {
  expect_true(spec_for(~ middle(Thumb, .95))$greedy)
  expect_false(spec_for(~ middle(Thumb, .95, greedy = FALSE))$greedy)
})

test_that("the proportion resolves in the environment the fill was written in", {
  local({
    my_prop <- .80
    # not spec_for(): gf_histogram() stamps the quosure's env to its own caller,
    # so routing through the shared helper would test the helper's frame instead
    p <- gf_histogram(~Thumb, data = Fingers, binwidth = 5, fill = ~ middle(Thumb, my_prop))
    expect_equal(cutoff_spec(plot_spec(p)$resolve_aes("fill"))$prop, .80)
  })
})

test_that("a fill that is not a distribution function is refused by name", {
  p <- gf_histogram(~Thumb, data = Fingers, fill = ~Sex)
  expect_error(cutoff_spec(plot_spec(p)$resolve_aes("fill")), "middle")
  expect_error(cutoff_spec(NULL), "fill")
})

test_that("a call to a non-distribution function is refused by name", {
  # tail_size() is a package internal, so without the whitelist get() would resolve it
  expect_error(spec_for(~ tail_size(Thumb, .05)), "middle, tails, upper, lower, outer")
  expect_error(spec_for(~ stats::median(Thumb)), "median\\(Thumb\\)")
})

test_that("a proportion that is not a single number in (0, 1) is refused", {
  expect_error(spec_for(~ middle(Thumb, 1.5)), "between 0 and 1")
  expect_error(spec_for(~ middle(Thumb, 0)), "between 0 and 1")
  expect_error(spec_for(~ middle(Thumb, "0.5")), "between 0 and 1")
  expect_error(spec_for(~ middle(Thumb, c(.1, .2))), "between 0 and 1")
  expect_error(spec_for(~ middle(Thumb, NA_real_)), "between 0 and 1")
})

plan_for <- function(func, prop, x, greedy = NA) {
  cutoff_plan(list(func = func, prop = prop, greedy = greedy), x)
}

# stated against middle()'s own output because tails() and outer() invert the colouring
in_middle <- function(x, func, prop) {
  middle(x, if (func == "outer") 1 - prop else prop)
}
edge_of <- function(marked, i, outward) {
  n <- length(marked)
  out <- i + outward
  isTRUE(marked[i]) && (out < 1 || out > n || !isTRUE(marked[out]))
}

test_that("a one-sided marker is the innermost shaded value of its tail", {
  set.seed(913)
  for (n in c(20, 39, 40, 100, 157, 200, 1000)) {
    x <- sort(rnorm(n))
    for (func in c("upper", "lower")) {
      plan <- plan_for(func, .05, x)
      shaded <- do.call(func, list(x, .05))
      side <- if (func == "upper") "upper" else "lower"
      i <- which(x == plan[[side]])[1]
      # outward = toward the middle, which is the direction the tail ends
      expect_true(
        edge_of(shaded, i, if (func == "upper") -1L else 1L),
        label = glue("{func} n={n}: marker at index {i} is the innermost shaded value")
      )
    }
  }
})

test_that("a two-sided marker is the edge of the middle region", {
  set.seed(913)
  for (n in c(20, 39, 40, 100, 157, 200, 1000)) {
    x <- sort(rnorm(n))
    for (func in c("middle", "tails", "outer")) {
      prop <- if (func == "outer") .05 else .95
      plan <- plan_for(func, prop, x)
      marked <- in_middle(x, func, prop)
      expect_true(
        edge_of(marked, which(x == plan$lower)[1], -1L),
        label = glue("{func} n={n}: lower marker is the low edge of the middle")
      )
      expect_true(
        edge_of(marked, which(x == plan$upper)[1], 1L),
        label = glue("{func} n={n}: upper marker is the high edge of the middle")
      )
    }
  }
})

test_that("middle, tails and outer put their markers in the same place", {
  x <- sort(rnorm(157))
  m <- plan_for("middle", .95, x)
  t <- plan_for("tails", .95, x)
  o <- plan_for("outer", .05, x)
  expect_equal(c(m$lower, m$upper), c(t$lower, t$upper))
  expect_equal(c(m$lower, m$upper), c(o$lower, o$upper))
})

test_that("one-sided fills produce one marker", {
  x <- sort(rnorm(100))
  expect_true(is.na(plan_for("upper", .05, x)$lower))
  expect_false(is.na(plan_for("upper", .05, x)$upper))
  expect_false(is.na(plan_for("lower", .05, x)$lower))
  expect_true(is.na(plan_for("lower", .05, x)$upper))
})

test_that("a non-greedy one-sided tail still marks its innermost shaded value", {
  x <- 1:100
  expect_equal(tail_size(x, .05, greedy = FALSE), 5)
  expect_equal(plan_for("lower", .05, x, greedy = FALSE)$lower, 5)
  expect_equal(plan_for("upper", .05, x, greedy = FALSE)$upper, 96)
})

test_that("a one-sided tail that rounds to no values marks the extreme value", {
  x <- 1:10
  expect_equal(tail_size(x, .05, greedy = FALSE), 0)
  expect_equal(plan_for("lower", .05, x, greedy = FALSE)$lower, 1)
  expect_equal(plan_for("upper", .05, x, greedy = FALSE)$upper, 10)
})

test_that("the tail proportion is the tail's share, not the fill's argument", {
  x <- sort(rnorm(100))
  expect_equal(plan_for("middle", .95, x)$tail_prop, .025)
  expect_equal(plan_for("tails", .95, x)$tail_prop, .025)
  expect_equal(plan_for("outer", .05, x)$tail_prop, .025)
  expect_equal(plan_for("upper", .05, x)$tail_prop, .05)
})

test_that("the label drops the leading zero for the proportions we teach", {
  x <- sort(rnorm(100))
  expect_equal(plan_for("middle", .95, x)$label, ".025")
  expect_equal(plan_for("middle", .90, x)$label, ".05")
  expect_equal(plan_for("upper", .01, x)$label, ".01")
})

test_that("the label keeps a tiny tail's true digits rather than rounding it away", {
  x <- sort(rnorm(100))
  expect_equal(plan_for("middle", .999999, x)$label, ".0000005")
})

test_that("data with no usable values is refused rather than drawn at NA", {
  expect_error(plan_for("middle", .95, numeric(0)), "no non-missing")
  expect_error(plan_for("middle", .95, c(NA_real_, NA_real_)), "no non-missing")
})
