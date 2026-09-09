cutoff_test_plot <- function(guide, aesthetic = "x", scale = NULL) {
  values <- data.frame(x = c(1, 10), y = c(10, 100))
  plot <- ggplot2::ggplot(values, ggplot2::aes(x, y)) + ggplot2::geom_point()
  if (is.null(scale)) {
    scale <- if (identical(aesthetic, "x")) {
      ggplot2::scale_x_continuous(guide = guide)
    } else {
      ggplot2::scale_y_continuous(guide = guide)
    }
  }
  plot + scale
}

cutoff_test_axis <- function(plot, side) {
  table <- ggplot2::ggplotGrob(plot)
  table$grobs[[which(table$layout$name == paste0("axis-", side))]]
}

cutoff_test_grobs <- function(grob, class) {
  found <- if (inherits(grob, class)) list(grob) else list()
  children <- list()
  if (!is.null(grob$children)) children <- c(children, as.list(grob$children))
  if (!is.null(grob$grobs)) children <- c(children, grob$grobs)
  for (child in children) {
    found <- c(found, cutoff_test_grobs(child, class))
  }
  found
}

cutoff_test_screen_position <- function(plot, value, aesthetic = "x") {
  panel <- ggplot2::ggplot_build(plot)$layout$panel_params[[1L]][[aesthetic]]
  transformed <- panel$scale$get_transformation()$transform(value)
  scales::rescale(transformed, from = panel$continuous_range)
}

test_that("guide_cutoff has the specified constructor and x/y availability", {
  expect_identical(
    names(formals(guide_cutoff)),
    c(
      "value", "label", "side", "call_id", "colour", "shape", "size",
      "linewidth", "title", "theme", "order", "position"
    )
  )

  guide <- guide_cutoff(c(2, 8))
  expect_s3_class(guide, "GuideCutoff")
  expect_identical(guide$available_aes, c("x", "y"))
  expect_identical(guide$params$value, c(2, 8))
  expect_identical(guide$params$label, list(NULL, NULL))
  expect_identical(guide$params$side, c(NA_character_, NA_character_))
  expect_identical(guide$params$call_id, 1L)
  expect_identical(guide$params$colour, "#1e3a8a")
  expect_identical(guide$params$shape, 24)
  expect_identical(guide$params$size, 4)
  expect_identical(guide$params$linewidth, 0.5)
})

test_that("guide_cutoff validates values, labels, sides, identity, and styling", {
  expect_error(guide_cutoff(numeric()), "one or two finite numbers")
  expect_error(guide_cutoff("2"), "one or two finite numbers")
  expect_error(guide_cutoff(c(1, 2, 3)), "one or two finite numbers")
  expect_error(guide_cutoff(c(1, Inf)), "one or two finite numbers")
  expect_error(guide_cutoff(1, label = "low"), "supplied together")
  expect_error(guide_cutoff(1, side = "lower"), "supplied together")
  expect_error(
    guide_cutoff(c(1, 2), label = "low", side = c("lower", "upper")),
    "label.*parallel"
  )
  expect_error(
    guide_cutoff(c(1, 2), label = c("low", "high"), side = "lower"),
    "side.*parallel"
  )
  expect_error(
    guide_cutoff(c(1, 2), label = c("one", "two"), side = c("lower", "lower")),
    "at most one cutoff"
  )
  expect_error(
    guide_cutoff(1, label = "one", side = "middle"),
    "lower.*upper"
  )
  expect_error(guide_cutoff(1, call_id = 0), "positive integer")
  expect_error(guide_cutoff(1, call_id = 1.5), "positive integer")
  expect_error(guide_cutoff(1, colour = c("red", "blue")), "one colour")
  expect_error(guide_cutoff(1, shape = c(24, 25)), "one point shape")
  expect_error(guide_cutoff(1, size = -1), "non-negative")
  expect_error(guide_cutoff(1, linewidth = Inf), "non-negative")
  expect_error(guide_cutoff(1, title = c("a", "b")), "one label")

  expect_no_error(guide_cutoff(
    c(1, 2), label = expression(alpha, beta),
    side = c("lower", "upper"), call_id = 2L, title = quote(gamma)
  ))
})

test_that("a direct cutoff guide builds and retains its key on x and y positions", {
  x_guide <- guide_cutoff(
    c(2, 8), label = c("lower label", "upper label"),
    side = c("lower", "upper"), call_id = 3L,
    colour = "purple", shape = 25, size = 5, linewidth = 1.25
  )
  y_guide <- guide_cutoff(
    c(20, 80), label = c("lower label", "upper label"),
    side = c("lower", "upper"), call_id = 4L
  )
  x_plot <- cutoff_test_plot(x_guide, "x")
  y_plot <- cutoff_test_plot(y_guide, "y")

  expect_s3_class(ggplot2::ggplotGrob(x_plot), "gtable")
  expect_s3_class(ggplot2::ggplotGrob(y_plot), "gtable")

  x_key <- ggplot2::get_guide_data(x_plot, "x")
  y_key <- ggplot2::get_guide_data(y_plot, "y")
  expect_true(all(
    c(
      ".value", ".label", ".visible", "side", "call_id", "colour",
      "shape", "size", "linewidth"
    ) %in% names(x_key)
  ))
  expect_identical(x_key$.value, c(2, 8))
  expect_identical(unclass(x_key$.label), list("lower label", "upper label"))
  expect_identical(x_key$side, c("lower", "upper"))
  expect_identical(x_key$call_id, c(3L, 3L))
  expect_identical(x_key$colour, c("purple", "purple"))
  expect_identical(unlist(x_key$shape), c(25, 25))
  expect_identical(x_key$size, c(5, 5))
  expect_identical(x_key$linewidth, c(1.25, 1.25))
  expect_identical(y_key$.value, c(20, 80))
  expect_identical(y_key$call_id, c(4L, 4L))
})

test_that("cutoff anchors keep their meaning on identity, log, and reverse scales", {
  values <- data.frame(x = c(1, 10), y = 1:2)
  make <- function(scale) {
    ggplot2::ggplot(values, ggplot2::aes(x, y)) + ggplot2::geom_point() + scale
  }
  guides <- function() {
    guide_cutoff(
      c(2, 8), label = c("lower", "upper"), side = c("lower", "upper")
    )
  }
  identity <- make(ggplot2::scale_x_continuous(guide = guides()))
  logged <- make(ggplot2::scale_x_log10(guide = guides()))
  reversed <- make(ggplot2::scale_x_reverse(guide = guides()))

  for (plot in list(identity, logged, reversed)) {
    key <- ggplot2::get_guide_data(plot, "x")
    expect_identical(key$.value, c(2, 8))
    expect_identical(unclass(key$.label), list("lower", "upper"))
    expect_identical(key$side, c("lower", "upper"))
    expect_true(all(key$.visible))
    expect_equal(key$x, cutoff_test_screen_position(plot, c(2, 8)))
  }
  expect_lt(ggplot2::get_guide_data(identity, "x")$x[[1L]],
            ggplot2::get_guide_data(identity, "x")$x[[2L]])
  expect_lt(ggplot2::get_guide_data(logged, "x")$x[[1L]],
            ggplot2::get_guide_data(logged, "x")$x[[2L]])
  expect_gt(ggplot2::get_guide_data(reversed, "x")$x[[1L]],
            ggplot2::get_guide_data(reversed, "x")$x[[2L]])
})

test_that("cutoff anchors are omitted rather than moved by OOB handling", {
  values <- data.frame(x = 1:10, y = 1:10)
  key_for <- function(oob) {
    plot <- ggplot2::ggplot(values, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(
        limits = c(1, 10), oob = oob,
        guide = guide_cutoff(
          c(0, 11), label = c("low", "high"), side = c("lower", "upper")
        )
      )
    expect_no_warning(key <- ggplot2::get_guide_data(plot, "x"))
    key
  }
  midpoint <- function(x, range) {
    outside <- x < range[[1L]] | x > range[[2L]]
    x[outside] <- mean(range)
    x
  }
  keep <- function(x, range) x

  censored <- key_for(scales::censor)
  squished <- key_for(scales::squish)
  custom <- key_for(midpoint)
  kept <- key_for(keep)

  for (key in list(censored, squished, custom, kept)) {
    expect_false(any(key$.visible))
    expect_true(all(is.na(key$x)))
    expect_identical(key$.value, c(0, 11))
  }
})

test_that("cutoff anchors exactly on edges remain visible", {
  values <- data.frame(x = c(1, 10), y = 1:2)
  plot <- ggplot2::ggplot(values, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(
      limits = c(1, 10), expand = ggplot2::expansion(mult = 0),
      guide = guide_cutoff(
        c(1, 10), label = c("edge low", "edge high"),
        side = c("lower", "upper")
      )
    )
  key <- ggplot2::get_guide_data(plot, "x")

  expect_true(all(key$.visible))
  expect_equal(key$x, c(0, 1))
  expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
})

test_that("coincident cutoffs keep truthful anchors and dodge labels in one lane", {
  guide <- guide_cutoff(
    c(5, 5), label = c("same lower", "same upper"),
    side = c("lower", "upper")
  )
  plot <- cutoff_test_plot(guide)
  key <- ggplot2::get_guide_data(plot, "x")
  axis <- cutoff_test_axis(plot, "b")
  texts <- cutoff_test_grobs(axis, "text")
  texts <- texts[vapply(
    texts, function(x) x$label %in% c("same lower", "same upper"), logical(1)
  )]
  texts <- texts[match(
    c("same lower", "same upper"),
    vapply(texts, function(x) x$label, character(1))
  )]
  leaders <- cutoff_test_grobs(axis, "segments")

  expect_true(all(key$.visible))
  expect_equal(key$x[[1L]], key$x[[2L]])
  expect_length(texts, 2)
  expect_length(leaders, 2)
  expect_equal(
    vapply(texts, function(x) grid::convertX(x$x, "npc", valueOnly = TRUE), 0),
    c(0.46, 0.54)
  )
  expect_equal(
    grid::convertY(texts[[1L]]$y, "npc", valueOnly = TRUE),
    grid::convertY(texts[[2L]]$y, "npc", valueOnly = TRUE)
  )
  expect_equal(
    vapply(leaders, function(x) grid::convertX(x$x0, "npc", valueOnly = TRUE), 0),
    rep(key$x[[1L]], 2)
  )
})

test_that("reverse scales dodge two-sided labels by screen order", {
  values <- data.frame(x = c(1, 10), y = 1:2)
  plot <- ggplot2::ggplot(values, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_reverse(guide = guide_cutoff(
      c(2, 8), label = c("lower", "upper"), side = c("lower", "upper")
    ))
  axis <- cutoff_test_axis(plot, "b")
  texts <- cutoff_test_grobs(axis, "text")
  texts <- texts[vapply(
    texts, function(x) x$label %in% c("lower", "upper"), logical(1)
  )]
  positions <- setNames(
    vapply(texts, function(x) grid::convertX(x$x, "npc", valueOnly = TRUE), 0),
    vapply(texts, function(x) x$label, character(1))
  )

  expect_equal(unname(positions[["upper"]]), 0.46)
  expect_equal(unname(positions[["lower"]]), 0.54)
})

test_that("markers and labels reserve measured guide space on both orientations", {
  x_unlabelled <- cutoff_test_plot(guide_cutoff(5, size = 4))
  x_labelled <- cutoff_test_plot(guide_cutoff(
    5, label = "cutoff", side = "lower", size = 4
  ))
  x_larger <- cutoff_test_plot(guide_cutoff(5, size = 8))
  y_unlabelled <- cutoff_test_plot(guide_cutoff(50, size = 4), "y")
  y_labelled <- cutoff_test_plot(guide_cutoff(
    50, label = "cutoff", side = "lower", size = 4
  ), "y")
  x_height <- function(plot) {
    as.numeric(grid::convertHeight(
      grid::grobHeight(cutoff_test_axis(plot, "b")), "cm"
    ))
  }
  y_width <- function(plot) {
    as.numeric(grid::convertWidth(
      grid::grobWidth(cutoff_test_axis(plot, "l")), "cm"
    ))
  }

  expect_gt(x_height(x_unlabelled), 0)
  expect_gt(x_height(x_labelled), x_height(x_unlabelled))
  expect_gt(x_height(x_larger), x_height(x_unlabelled))
  expect_gt(y_width(y_unlabelled), 0)
  expect_gt(y_width(y_labelled), y_width(y_unlabelled))

  marked_axis <- cutoff_test_axis(cutoff_test_plot(guide_cutoff(
    c(2, 8), label = c("low", "high"), side = c("lower", "upper")
  )), "b")
  expect_length(cutoff_test_grobs(marked_axis, "polygon"), 2)
  expect_length(cutoff_test_grobs(marked_axis, "segments"), 2)
  expect_setequal(
    vapply(cutoff_test_grobs(marked_axis, "text"), function(x) x$label, character(1)),
    c("low", "high")
  )
})

test_that("directional markers stay inside their measured rail", {
  marker <- function(position) {
    position_anchor_grob(
      at = 0.5, position = position, shape = ggplot2::waiver(), size = 4,
      colour = "blue", linewidth = 0.5
    )
  }

  expect_equal(as.numeric(marker("top")$vp$y), 0.5)
  expect_equal(as.numeric(marker("bottom")$vp$y), 0.5)
  expect_equal(as.numeric(marker("left")$vp$x), 0.5)
  expect_equal(as.numeric(marker("right")$vp$x), 0.5)
  expect_equal(marker("top")$vp$just, c(0.5, 0.5))
  expect_equal(marker("bottom")$vp$just, c(0.5, 0.5))
})

test_that("a y-position cutoff guide keeps native horizontal text", {
  label_theme <- ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      colour = "blue", size = 3.2 * ggplot2::.pt, face = "italic"
    ),
    axis.text.y = ggplot2::element_text(
      colour = "blue", size = 3.2 * ggplot2::.pt, face = "italic"
    )
  )
  plot <- cutoff_test_plot(guide_cutoff(
    c(20, 80), label = c("low", "high"), side = c("lower", "upper"),
    theme = label_theme
  ), "y")
  labels <- cutoff_test_grobs(cutoff_test_axis(plot, "l"), "text")
  labels <- labels[vapply(
    labels, function(grob) grob$label %in% c("low", "high"), logical(1)
  )]

  expect_length(labels, 2)
  expect_identical(vapply(labels, function(grob) grob$rot, numeric(1)), c(0, 0))
})
