test_that("resid_jitter pins an unseeded jitter, on the plot it returns", {
  plot <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1)
  expect_false(is.finite(plot$layers[[1]]$position$seed))

  out <- resid_jitter(plot)

  expect_true(is.finite(out$plot$layers[[1]]$position$seed))
  expect_s3_class(out$position, "PositionResidJitter")
  expect_equal(out$position$seed, out$plot$layers[[1]]$position$seed)
})

test_that("resid_jitter leaves the caller's own plot alone", {
  # the old design pinned the caller's plot in place so a later overlay would
  # land on the same dots. Chaining gives that anyway -- the pinned plot is what
  # the pipeline passes on -- and it costs the caller nothing they did not ask for
  plot <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1)
  invisible(resid_jitter(plot))

  expect_false(is.finite(plot$layers[[1]]$position$seed))
})

test_that("resid_jitter keeps a seed the caller chose", {
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Sex, Thumb)) +
    ggplot2::geom_point(position = ggplot2::position_jitter(seed = 42))

  out <- resid_jitter(plot)

  expect_identical(out$plot$layers[[1]]$position$seed, 42)
  expect_identical(out$position$seed, 42)
})

test_that("resid_jitter does not write to ggplot2's shared position object", {
  # geom_jitter() is handed ggplot2's shared PositionJitter, not an instance, so
  # seeding it in place would seed every jitter drawn afterwards in the session
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Sex, Thumb)) + ggplot2::geom_jitter()
  skip_if_not(identical(plot$layers[[1]]$position, ggplot2::PositionJitter))

  out <- resid_jitter(plot)

  expect_false(is.finite(ggplot2::PositionJitter$seed))
  expect_true(is.finite(out$plot$layers[[1]]$position$seed))
})

test_that("a plot with no jitter needs no offset", {
  plot <- gf_point(Thumb ~ Height, data = Fingers)
  out <- resid_jitter(plot)

  expect_identical(out$plot, plot)
  expect_identical(out$position, "identity")
})

test_that("with_random_seed_restored leaves the caller's stream where it found it", {
  set.seed(42)
  a <- runif(3)
  set.seed(42)
  with_random_seed_restored(runif(10))
  b <- runif(3)

  # MUTATION: dropping the on.exit() restore, which silently moves a reader's
  # sampling distribution when they pipe gf_model() onto a plot.
  expect_equal(a, b)
})

test_that("with_random_seed_restored leaves no seed behind when it found none", {
  # MUTATION: restoring with assign() unconditionally, even with nothing to
  # restore -- that would plant a stream in a session that never had one.
  if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  with_random_seed_restored(runif(1))

  expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
})
