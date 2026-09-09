test_that("an implicit continuous position scale is materialized without changing training", {
  base <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  expect_null(base$scales$get_scales("x"))

  state <- position_guide_state(base, "x")
  expect_null(base$scales$get_scales("x"))
  expect_s3_class(state$scale, "ScaleContinuousPosition")

  before <- ggplot2::ggplot_build(base)$layout$panel_params[[1]]$x
  after <- ggplot2::ggplot_build(state$plot)$layout$panel_params[[1]]$x
  expect_equal(after$continuous_range, before$continuous_range)
  expect_equal(after$get_limits(), before$get_limits())
  expect_equal(after$get_breaks(), before$get_breaks())
  expect_equal(after$get_labels(), before$get_labels())
  expect_equal(after$position, before$position)
})

test_that("scale and plot guide objects are detached before composition", {
  axis <- ggplot2::guide_axis(angle = 17)
  base <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(limits = c(1, 6), breaks = c(2, 4, 6)) +
    ggplot2::guides(x = axis)
  original_scale <- base$scales$get_scales("x")
  original_guides <- base$guides

  out <- add_dgp_position_guides(
    base, guide_dgp(role = "estimate"), guide_dgp(role = "population")
  )
  out_scale <- out$scales$get_scales("x")

  expect_identical(base$scales$get_scales("x"), original_scale)
  expect_identical(base$guides, original_guides)
  expect_true("x" %in% names(base$guides$guides))
  expect_false("x" %in% names(out$guides$guides))
  expect_s3_class(original_scale$guide, "waiver")
  expect_s3_class(out_scale$guide, "GuideAxisStack")
  expect_identical(out_scale$limits, c(1, 6))
  expect_identical(out_scale$breaks, c(2, 4, 6))
  expect_identical(out_scale$guide$params$guides[[1]]$params$angle, 17)
})

test_that("a named NULL guide override remains explicit suppression", {
  base <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::guides(x = NULL)
  expect_true("x" %in% names(base$guides$guides))

  out <- add_dgp_position_guides(
    base, guide_dgp(role = "estimate"), guide_dgp(role = "population")
  )
  guide <- out$scales$get_scales("x")$guide

  expect_s3_class(guide, "GuideDgp")
  expect_true("x" %in% names(base$guides$guides))
})

test_that("moving a position override preserves unrelated caller guides", {
  legend <- ggplot2::guide_legend(reverse = TRUE)
  base <- ggplot2::ggplot(
    mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))
  ) +
    ggplot2::geom_point() +
    ggplot2::guides(x = ggplot2::guide_axis(angle = 17), colour = legend)

  out <- add_dgp_position_guides(
    base, guide_dgp(role = "estimate"), guide_dgp(role = "population")
  )

  expect_setequal(names(base$guides$guides), c("x", "colour"))
  expect_identical(names(out$guides$guides), "colour")
  expect_true(out$guides$guides$colour$params$reverse)
  expect_no_error(ggplot2::ggplotGrob(out))
})

test_that("an existing axis stack is rebuilt flat with its settings", {
  caller <- ggplot2::guide_axis_stack(
    ggplot2::guide_axis(angle = 17),
    ggplot2::guide_axis(minor.ticks = TRUE),
    spacing = grid::unit(2, "mm"), order = 3, position = "bottom"
  )
  base <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() + ggplot2::scale_x_continuous(guide = caller)

  out <- add_dgp_position_guides(
    base, guide_dgp(role = "estimate"), guide_dgp(role = "population")
  )
  stack <- out$scales$get_scales("x")$guide

  expect_s3_class(stack, "GuideAxisStack")
  expect_length(stack$params$guides, 3)
  expect_false(any(vapply(stack$params$guides, inherits, logical(1), "GuideAxisStack")))
  expect_identical(stack$params$guides[[1]]$params$angle, 17)
  expect_true(stack$params$guides[[2]]$params$minor.ticks)
  expect_equal(stack$params$spacing, grid::unit(2, "mm"))
  expect_identical(stack$params$order, 3L)
  expect_identical(stack$params$position, "bottom")
})

test_that("DGP guides are scale-owned and a later scale replacement wins", {
  base <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  estimate <- dgp_upright_guide(guide_dgp(role = "estimate"))
  population <- dgp_upright_guide(guide_dgp(role = "population"))
  out <- add_dgp_position_guides(base, estimate, population)
  scale <- out$scales$get_scales("x")

  expect_s3_class(scale$guide, "GuideAxisStack")
  expect_length(position_guide_matches(scale$guide, "GuideDgp", "estimate"), 1)
  expect_s3_class(scale$secondary.axis, "AxisSecondary")
  expect_length(position_guide_matches(scale$secondary.axis, "GuideDgp", "population"), 1)

  replaced <- suppressMessages(out + ggplot2::scale_x_continuous())
  replacement <- replaced$scales$get_scales("x")
  expect_s3_class(replacement$guide, "waiver")
  expect_s3_class(replacement$secondary.axis, "waiver")
})

test_that("DGP installation refuses reversed teaching sides", {
  estimate <- guide_dgp(role = "estimate")
  population <- guide_dgp(role = "population")
  top <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() + ggplot2::scale_x_continuous(position = "top")
  secondary <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis())

  expect_error(
    add_dgp_position_guides(top, estimate, population),
    "primary x guide at the bottom"
  )
  expect_error(
    add_dgp_position_guides(secondary, estimate, population),
    "secondary x position"
  )
})

test_that("repeated builds neither expand scales nor accumulate guide children", {
  values <- data.frame(x = c(-3, -1, 2, 4))
  base <- ggplot2::ggplot(values, ggplot2::aes(x)) +
    ggplot2::geom_histogram(bins = 4)
  out <- add_dgp_position_guides(
    base, guide_dgp(value = 0, role = "estimate"),
    guide_dgp(value = 0, role = "population")
  )
  before <- ggplot2::ggplot_build(base)$layout$panel_params[[1]]$x.range
  expected_children <- length(out$scales$get_scales("x")$guide$params$guides)

  invisible(ggplot2::ggplotGrob(out))
  invisible(ggplot2::ggplotGrob(out))

  expect_equal(
    ggplot2::ggplot_build(out)$layout$panel_params[[1]]$x.range,
    before
  )
  expect_length(out$scales$get_scales("x")$guide$params$guides, expected_children)
})

test_that("flipped guide state reads suppression from the physical aesthetic", {
  values <- data.frame(x = c(-3, -1, 2, 4))
  base <- ggplot2::ggplot(values, ggplot2::aes(x)) +
    ggplot2::geom_histogram(bins = 4) + ggplot2::coord_flip() +
    ggplot2::guides(y = "none")

  state <- position_guide_state(base, "x")
  expect_identical(state$physical, "y")
  expect_true(state$from_override)
  expect_true(guide_is_suppressed(state$guide))
})
