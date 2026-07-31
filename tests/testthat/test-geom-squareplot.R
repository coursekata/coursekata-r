bins_for <- function(x, ...) {
  params <- StatSquareplot$setup_params(data.frame(x = x), list(...))
  do.call(
    StatSquareplot$compute_group,
    c(list(data = data.frame(x = x), scales = NULL), params)
  )
}

test_that("integer data over a small range gets one bin per integer", {
  out <- bins_for(c(1, 1, 2, 2, 2, 3, 3, 4, 5, 5))
  expect_equal(out$xmin, 1:5)
  expect_equal(out$count, c(2, 3, 2, 1, 2))
})

test_that("the largest value gets its own bin rather than folding into the one below", {
  out <- bins_for(1:5)
  expect_equal(nrow(out), 5)
  expect_equal(out$count, rep(1, 5))
})

test_that("integer data past the small-range cutoff falls back to thirtieths", {
  expect_equal(unique(bins_for(c(0L, 50L))$binwidth), 1)
  expect_equal(unique(bins_for(c(0L, 51L))$binwidth), 51 / 30)
})

test_that("non-integer data over a small range still gets thirtieths", {
  expect_equal(unique(bins_for(c(1.5, 2.25, 3.75, 10.5))$binwidth), 9 / 30)
})

test_that("a single repeated value still produces one bin", {
  out <- bins_for(rep(5, 10))
  expect_equal(nrow(out), 1)
  expect_equal(out$count, 10)
})

test_that("an explicit binwidth and origin are honoured", {
  out <- bins_for(c(1, 3, 5, 7), binwidth = 2, origin = 0)
  expect_equal(out$xmin, c(0, 2, 4, 6))
  expect_equal(out$count, rep(1, 4))
})

test_that("empty bins between occupied ones are kept", {
  out <- bins_for(c(1, 1, 4, 4))
  expect_equal(out$count, c(2, 0, 0, 2))
})

test_that("drop = TRUE removes the empty bins between occupied ones", {
  out <- bins_for(c(1, 1, 4, 4), drop = TRUE)
  expect_equal(nrow(out), 2)
  expect_equal(out$xmin, c(1, 4))
  expect_equal(out$count, c(2, 2))
})

test_that("a layer with nothing to bin yields no bins and no warning", {
  expect_no_warning(out <- bins_for(numeric(0)))
  expect_equal(nrow(out), 0)
  expect_named(out, c("count", "x", "xmin", "xmax", "binwidth", "width"))
})

test_that("a layer of nothing but missing values is treated as empty", {
  expect_no_warning(out <- bins_for(c(NA_real_, NA_real_)))
  expect_equal(nrow(out), 0)
})

test_that("setup_params refuses to let binaxis be anything but x", {
  params <- StatSquareplot$setup_params(data.frame(x = c(1, 2, 3)), list(binaxis = "y"))
  expect_equal(params$binaxis, "x")
})

expand_for <- function(counts, xmin, params = list()) {
  data <- data.frame(
    count = counts, xmin = xmin, xmax = xmin + 1,
    binwidth = 1, PANEL = factor(1), group = 1L
  )
  GeomSquareplot$setup_data(data, params)
}

test_that("each observation becomes one rectangle", {
  out <- expand_for(c(2, 3, 1), c(1, 2, 3))
  expect_equal(nrow(out), 6)
})

test_that("rectangles stack from zero upwards within a bin", {
  out <- expand_for(c(3), c(1))
  expect_equal(out$ymin, c(0, 1, 2))
  expect_equal(out$ymax, c(1, 2, 3))
})

test_that("each rectangle is one unit tall so the y axis stays a count", {
  out <- expand_for(c(4), c(1))
  expect_true(all(out$ymax - out$ymin == 1))
  expect_equal(max(out$ymax), 4)
})

test_that("an empty bin contributes no rectangles", {
  out <- expand_for(c(2, 0, 1), c(1, 2, 3))
  expect_equal(nrow(out), 3)
  expect_equal(sort(unique(out$xmin)), c(1, 3))
})

test_that("bins are stacked independently of each other", {
  out <- expand_for(c(2, 2), c(1, 2))
  expect_equal(out$ymin[out$xmin == 1], c(0, 1))
  expect_equal(out$ymin[out$xmin == 2], c(0, 1))
})

test_that("a mapped fill stacks its groups within a bin instead of overplotting", {
  d <- data.frame(
    x = c(1, 1, 1, 2, 2),
    g = c("a", "a", "b", "a", "b")
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, fill = .data$g)) +
    ggplot2::layer(geom = GeomSquareplot, stat = StatSquareplot, position = "identity")
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(built$ymin[built$xmin == 1], c(0, 1, 2))
  expect_equal(built$ymin[built$xmin == 2], c(0, 1))
  expect_equal(built$group[built$xmin == 1], c(1L, 1L, 2L))
})

test_that("a bin's squares stack in group order however the rows arrive", {
  data <- data.frame(
    count = 1, xmin = c(1, 1, 1, 2, 2), xmax = c(2, 2, 2, 3, 3),
    binwidth = 1, PANEL = factor(1), group = c(2L, 1L, 2L, 2L, 1L)
  )
  out <- GeomSquareplot$setup_data(data, list())
  expect_equal(out$group[out$xmin == 1], c(1L, 2L, 2L))
  expect_equal(out$ymin[out$xmin == 1], c(0, 1, 2))
})

required_aes_of <- function(geom) {
  # ggplot2 spells an either/or requirement as one string, "x|width|xmin|xmax"
  unlist(strsplit(geom$required_aes, "|", fixed = TRUE))
}

test_that("the geom does not require the rectangle corners it builds itself", {
  expect_false(any(c("ymin", "ymax") %in% required_aes_of(GeomSquareplot)))
})

test_that("everything the geom requires is something the stat has already produced", {
  produced <- c(names(bins_for(c(1, 1, 2, 3))), names(StatSquareplot$default_aes))
  expect_true(all(required_aes_of(GeomSquareplot) %in% produced))
})

test_that("a squareplot layer builds from nothing but a mapped x", {
  p <- ggplot2::ggplot(data.frame(x = c(1, 1, 2, 2, 2, 3, 3)), ggplot2::aes(x = .data$x)) +
    ggplot2::layer(geom = GeomSquareplot, stat = StatSquareplot, position = "identity")
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(nrow(built), 7)
  expect_equal(max(built$ymax), 3)
})

test_that("the border is left alone while the square is comfortably bigger", {
  # linewidth 0.5 is a 1.42pt stroke; the smaller side caps at 3.4pt and 2.7pt here
  expect_equal(fit_square_border(13.6, 29.1, 0.5), 0.5 * ggplot2::.pt)
  expect_equal(fit_square_border(13.7, 10.9, 0.5), 0.5 * ggplot2::.pt)
})

test_that("the border is thinned once it would crowd the square", {
  expect_equal(fit_square_border(13.7, 4.0, 0.5), 1.0)
  expect_equal(fit_square_border(13.6, 1.2, 0.5), 0.3)
})

test_that("the border is fitted to whichever side of the square is smaller", {
  expect_equal(fit_square_border(40, 2, 0.5), 0.5)
  expect_equal(fit_square_border(2, 40, 0.5), 0.5)
})

test_that("an explicit linewidth is honoured exactly, never fitted", {
  expect_equal(fit_square_border(13.6, 1.2, 0.5, fit = FALSE), 0.5 * ggplot2::.pt)
})

test_that("the theme's borderwidth still reaches the squares", {
  skip_if_not_installed("ggplot2", "4.0.0")
  p <- ggplot2::ggplot(data.frame(x = c(1, 1, 2, 3)), ggplot2::aes(x = .data$x)) +
    ggplot2::layer(geom = GeomSquareplot, stat = StatSquareplot, position = "identity") +
    ggplot2::theme(geom = ggplot2::element_geom(borderwidth = 3))
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(unique(built$linewidth), 3)
  expect_true(all(built$fit_border))
})

test_that("a linewidth the caller supplied is marked as not needing a fit", {
  p <- ggplot2::ggplot(data.frame(x = c(1, 1, 2, 3)), ggplot2::aes(x = .data$x)) +
    ggplot2::layer(
      geom = GeomSquareplot, stat = StatSquareplot, position = "identity",
      params = list(linewidth = 2)
    )
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(unique(built$linewidth), 2)
  expect_false(any(built$fit_border))
})

squares_layer <- function(x, ...) {
  ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = .data$x)) +
    ggplot2::layer(
      geom = GeomSquareplot, stat = StatSquareplot, position = "identity",
      params = list(...)
    )
}

drawn_rects <- function(p) {
  grDevices::pdf(NULL, width = 7, height = 4.5)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(p)
  grid::grid.force()
  drawn <- grid::grid.get("geom_rect", grep = TRUE, global = TRUE)
  if (grid::is.grob(drawn)) list(drawn) else drawn
}

test_that("a bin's bar spans zero to that bin's own count", {
  bars <- squareplot_bar_rows(expand_for(c(2, 3, 1), c(1, 2, 3)))
  expect_equal(nrow(bars), 3)
  expect_equal(bars$xmin, c(1, 2, 3))
  expect_equal(bars$ymin, c(0, 0, 0))
  expect_equal(bars$ymax, c(2, 3, 1))
})

test_that("an empty bin gets no bar, the same way it gets no squares", {
  bars <- squareplot_bar_rows(expand_for(c(2, 0, 1), c(1, 2, 3)))
  expect_equal(nrow(bars), 2)
  expect_equal(bars$xmin, c(1, 3))
  expect_equal(bars$ymax, c(2, 1))
})

test_that("a bar mode the geom does not know is refused by name", {
  expect_error(
    ggplot2::ggplot_build(squares_layer(c(1, 1, 2), bar = "oops")),
    "none"
  )
})

test_that("every bar mode is one layer of one geom on one bin grid", {
  built <- lapply(c("none", "outline", "solid"), function(mode) {
    p <- squares_layer(c(1, 1, 2, 2, 2, 3), bar = mode)
    expect_equal(length(p$layers), 1)
    expect_s3_class(p$layers[[1]]$geom, "GeomSquareplot")
    ggplot2::ggplot_build(p)$data[[1]][, c("xmin", "xmax", "ymin", "ymax")]
  })
  expect_equal(built[[1]], built[[2]])
  expect_equal(built[[1]], built[[3]])
})

test_that("bar = 'none' draws the squares and nothing else", {
  gs <- drawn_rects(squares_layer(c(1, 1, 2, 2, 2, 3)))
  expect_length(gs, 1)
  expect_length(gs[[1]]$x, 6)
})

test_that("bar = 'solid' draws one bar per bin and no squares", {
  gs <- drawn_rects(squares_layer(c(1, 1, 2, 2, 2, 3), bar = "solid", fill = "coral"))
  expect_length(gs, 1)
  expect_length(gs[[1]]$x, 3)
  expect_equal(unique(gs[[1]]$gp$fill), "#FF7F50")
})

test_that("bar = 'outline' frames the squares without replacing them", {
  gs <- drawn_rects(squares_layer(c(1, 1, 2, 2, 2, 3), bar = "outline", fill = "coral"))
  expect_length(gs, 2)
  expect_length(gs[[1]]$x, 6)
  expect_length(gs[[2]]$x, 3)
  expect_equal(unique(gs[[1]]$gp$fill), "#FF7F50")
  expect_true(all(is.na(gs[[2]]$gp$fill)))
})

test_that("the bar's colour and width are its own, not the separators'", {
  gs <- drawn_rects(squares_layer(c(1, 1, 2), bar = "outline", colour = "white"))
  expect_equal(unique(gs[[1]]$gp$col), "white")
  expect_equal(unique(gs[[2]]$gp$col), "grey20")
  red <- drawn_rects(squares_layer(
    c(1, 1, 2), bar = "outline", colour = "white", bar_colour = "red", bar_linewidth = 2
  ))
  expect_equal(unique(red[[1]]$gp$col), "white")
  expect_equal(unique(red[[2]]$gp$col), "red")
  expect_equal(unique(red[[1]]$gp$lwd), 0.5 * ggplot2::.pt)
  expect_equal(unique(red[[2]]$gp$lwd), 2 * ggplot2::.pt)
})

test_that("the border fitting still runs when a bar is drawn over the squares", {
  set.seed(24)
  gs <- drawn_rects(squares_layer(rnorm(2000, 50, 10), bar = "outline"))
  expect_length(gs, 2)
  expect_length(gs[[1]]$x, 2000)
  # an unfired makeContent leaves the untouched 1.42pt stroke
  expect_length(gs[[1]]$gp$lwd, 1)
  expect_lt(gs[[1]]$gp$lwd, 0.5 * ggplot2::.pt)
  expect_equal(unique(gs[[2]]$gp$lwd), 0.5 * ggplot2::.pt)
})

test_that("each panel's bar is that panel's own squares added up", {
  d <- data.frame(x = c(1, 1, 2, 3, 3, 3), f = rep(c("a", "b"), each = 3))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x)) +
    ggplot2::layer(
      geom = GeomSquareplot, stat = StatSquareplot, position = "identity",
      params = list(bar = "outline")
    ) +
    ggplot2::facet_wrap(~f)
  gs <- drawn_rects(p)
  expect_length(gs, 4)
  expect_equal(vapply(gs, function(g) length(g$x), integer(1)), c(3L, 2L, 3L, 1L))
})

test_that("bar = 'solid' keeps its bin's own group composition", {
  d <- data.frame(x = c(1, 1, 1, 2, 2, 2), g = c("a", "a", "b", "a", "b", "b"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, fill = .data$g)) +
    ggplot2::layer(
      geom = GeomSquareplot, stat = StatSquareplot, position = "identity",
      params = list(bar = "solid")
    ) +
    ggplot2::scale_fill_manual(values = c(a = "#112233", b = "#445566"))
  gs <- drawn_rects(p)
  expect_length(gs, 1)
  # two bins of two groups each: four stacked spans, not two bars in one colour
  expect_length(gs[[1]]$x, 4)
  expect_equal(gs[[1]]$gp$fill, c("#112233", "#445566", "#112233", "#445566"))
  # 2 then 1 in the first bin, 1 then 2 in the second
  height <- as.numeric(gs[[1]]$height)
  expect_equal(height / min(height), c(2, 1, 1, 2))
  # GeomRect anchors a rectangle at its top-left corner
  bottom <- as.numeric(gs[[1]]$y) - height
  expect_equal(bottom[c(2, 4)], (bottom + height)[c(1, 3)])
  expect_equal(bottom[c(1, 3)], rep(min(bottom), 2))
  expect_equal(as.numeric(gs[[1]]$x)[c(1, 3)], as.numeric(gs[[1]]$x)[c(2, 4)])
})

test_that("bar = 'outline' frames the whole bin however many groups are in it", {
  d <- data.frame(x = c(1, 1, 1, 2, 2, 2), g = c("a", "a", "b", "a", "b", "b"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, fill = .data$g)) +
    ggplot2::layer(
      geom = GeomSquareplot, stat = StatSquareplot, position = "identity",
      params = list(bar = "outline")
    )
  gs <- drawn_rects(p)
  expect_length(gs, 2)
  expect_length(gs[[1]]$x, 6)
  expect_length(gs[[2]]$x, 2)
  # one frame around all three of a bin's squares, reaching the top one's top
  square <- mean(as.numeric(gs[[1]]$height))
  expect_equal(as.numeric(gs[[2]]$height), rep(3 * square, 2))
  expect_equal(as.numeric(gs[[2]]$y), rep(max(as.numeric(gs[[1]]$y)), 2))
})

test_that("a solid bar's spans are the groups its squares stacked in", {
  data <- data.frame(
    count = 1, xmin = c(1, 1, 1, 2, 2, 2), xmax = c(2, 2, 2, 3, 3, 3),
    binwidth = 1, PANEL = factor(1), group = c(1L, 1L, 2L, 1L, 2L, 2L)
  )
  spans <- squareplot_bar_rows(GeomSquareplot$setup_data(data, list()), by_group = TRUE)
  expect_equal(spans$xmin, c(1, 1, 2, 2))
  expect_equal(spans$group, c(1L, 2L, 1L, 2L))
  expect_equal(spans$ymin, c(0, 2, 0, 1))
  expect_equal(spans$ymax, c(2, 3, 1, 3))
})
