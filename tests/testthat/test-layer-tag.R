test_that("a tagged layer can be found by name after further composition", {
  seg <- data.frame(x = 1, y = 1, xend = 2, yend = 2)
  layer <- tag_layer(
    ggplot2::geom_segment(
      data = seg,
      mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      inherit.aes = FALSE
    ),
    "resid"
  )

  p <- gf_point(Thumb ~ Height, data = Fingers) + layer
  expect_equal(layer_index(p, "resid"), 2L)

  p <- p + ggplot2::geom_rug()
  expect_equal(layer_index(p, "resid"), 2L)
  expect_equal(length(p$layers), 3L)
})

test_that("layer_indices finds every layer carrying the tag, in plot order", {
  # the documented two-model comparison leaves two layers tagged "model"
  p <- gf_point(Thumb ~ Height, data = Fingers) %>%
    gf_model(lm(Thumb ~ NULL, data = Fingers), color = "dodgerblue") %>%
    gf_model(lm(Thumb ~ Height, data = Fingers), color = "firebrick")

  # would fail if layer_indices returned only the first hit (an alias for
  # layer_index()) or returned the hits out of plot order
  expect_identical(layer_indices(p, "model"), c(2L, 3L))
})

test_that("layer_indices is empty, not NA, when no layer carries the tag", {
  # would fail if layer_indices copied layer_index()'s NA branch, which would
  # make length(...) == 1 on a plot with no such layer
  expect_identical(layer_indices(gf_point(Thumb ~ Height, data = Fingers), "resid"), integer(0))
})

test_that("layer_index still answers with the first hit", {
  # the documented two-model comparison leaves two layers tagged "model"
  p <- gf_point(Thumb ~ Height, data = Fingers) %>%
    gf_model(lm(Thumb ~ NULL, data = Fingers), color = "dodgerblue") %>%
    gf_model(lm(Thumb ~ Height, data = Fingers), color = "firebrick")

  expect_equal(length(p$layers), 3L)
  # would fail if layer_index() were "simplified" into layer_indices(p, tag)[[1]],
  # which errors instead of returning NA on a miss (see next assertion)
  expect_equal(layer_index(p, "model"), 2L)
})

test_that("layer_index is NA when no layer carries the tag", {
  p <- gf_point(Thumb ~ Height, data = Fingers)
  # would fail (error instead of NA) if layer_index() were rewritten as
  # layer_indices(p, tag)[[1]]
  expect_identical(layer_index(p, "resid"), NA_integer_)
})
