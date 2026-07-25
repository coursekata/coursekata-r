test_that("freeze_jitter pins an unseeded jitter layer", {
  plot <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1)
  expect_false(is.finite(plot$layers[[1]]$position$seed))

  frozen <- freeze_jitter(plot)
  expect_true(is.finite(frozen$layers[[1]]$position$seed))
})

test_that("freeze_jitter leaves an already seeded layer alone", {
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Sex, Thumb)) +
    ggplot2::geom_point(position = ggplot2::position_jitter(seed = 42))

  expect_identical(freeze_jitter(plot)$layers[[1]]$position$seed, 42)
})

test_that("freeze_jitter freezes the plot it is handed", {
  # deliberate: overlays added to the base plot later must land on the same dots
  plot <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1)
  invisible(freeze_jitter(plot))

  expect_true(is.finite(plot$layers[[1]]$position$seed))
})

test_that("freeze_jitter does not write to ggplot2's shared position object", {
  # geom_jitter() is handed ggplot2's shared PositionJitter, not an instance
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Sex, Thumb)) + ggplot2::geom_jitter()
  skip_if_not(identical(plot$layers[[1]]$position, ggplot2::PositionJitter))

  frozen <- freeze_jitter(plot)

  expect_false(is.finite(ggplot2::PositionJitter$seed))
  expect_true(is.finite(frozen$layers[[1]]$position$seed))
})

test_that("freeze_jitter leaves plots without jitter untouched", {
  plot <- gf_point(Thumb ~ Height, data = Fingers)
  expect_identical(freeze_jitter(plot), plot)
})
