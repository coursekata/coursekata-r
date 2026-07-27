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

test_that("layer_index finds the first layer carrying the tag, not the last", {
  # the documented two-model comparison leaves two layers tagged "model"
  p <- gf_point(Thumb ~ Height, data = Fingers) %>%
    gf_model(lm(Thumb ~ NULL, data = Fingers), color = "dodgerblue") %>%
    gf_model(lm(Thumb ~ Height, data = Fingers), color = "firebrick")

  expect_equal(length(p$layers), 3L)
  expect_equal(layer_index(p, "model"), 2L)
})

test_that("layer_index is NA when no layer carries the tag", {
  p <- gf_point(Thumb ~ Height, data = Fingers)
  expect_identical(layer_index(p, "resid"), NA_integer_)
})
