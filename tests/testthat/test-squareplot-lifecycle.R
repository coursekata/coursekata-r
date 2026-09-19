squareplot_render_panel <- function(plot, width = 160, height = 100) {
  built <- ggplot2::ggplot_build(plot)
  data <- built$data[[length(built$data)]]
  grob <- GeomSquareplot$draw_panel(
    data, built$layout$panel_params[[1]], built$layout$coord
  )
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(
    width = grid::unit(width, "mm"), height = grid::unit(height, "mm")
  ))
  grid::grid.draw(grob)
  suppressWarnings(grid::grid.force())
  grid::grid.get("geom_rect", grep = TRUE, global = TRUE)
}

test_that("border fitting measures both physical sides after coordinate placement", {
  d <- data.frame(x = c(rep(1, 1500), rep(3, 4)))
  for (coord in list(ggplot2::coord_cartesian(), ggplot2::coord_flip(),
                    ggplot2::coord_transform(y = "sqrt"))) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x)) +
      geom_squareplot(breaks = c(0, 2, 5)) + coord
    built <- ggplot2::ggplot_build(p)
    placed <- built$layout$coord$transform(
      built$data[[1]], built$layout$panel_params[[1]]
    )
    for (size in list(c(160, 100), c(80, 120))) {
      drawn <- squareplot_render_panel(p, size[[1]], size[[2]])
      pt_per_mm <- 72.27 / 25.4
      smaller <- pmin(
        abs(placed$xmax - placed$xmin) * size[[1]],
        abs(placed$ymax - placed$ymin) * size[[2]]
      ) * pt_per_mm
      expect_length(drawn$gp$lwd, nrow(d))
      expect_equal(drawn$gp$lwd, pmin(built$data[[1]]$linewidth * ggplot2::.pt, smaller / 4))
      expect_true(all(drawn$gp$lwd <= smaller / 4 + 1e-10))
    }
  }
})

test_that("each bin's own width caps its border when width is the smaller side", {
  d <- data.frame(x = c(50, 100.05))
  p <- ggplot2::ggplot(d, ggplot2::aes(x)) +
    geom_squareplot(breaks = c(0, 100, 100.1))
  drawn <- squareplot_render_panel(p)
  built <- ggplot2::ggplot_build(p)
  placed <- built$layout$coord$transform(built$data[[1]], built$layout$panel_params[[1]])
  width_pt <- abs(placed$xmax - placed$xmin) * 160 * 72.27 / 25.4
  expect_equal(drawn$gp$lwd, pmin(built$data[[1]]$linewidth * ggplot2::.pt, width_pt / 4))
  expect_lt(drawn$gp$lwd[[2]], drawn$gp$lwd[[1]] / 5)
})

test_that("flipped fixed borders warn only for squares actually hidden", {
  d <- data.frame(x = rep(1, 10))
  base <- ggplot2::ggplot(d, ggplot2::aes(x)) + ggplot2::coord_flip()
  expect_no_warning(squareplot_render_panel(base + geom_squareplot(linewidth = 1)))
  expect_warning(
    squareplot_render_panel(base + geom_squareplot(linewidth = 20)),
    "10 observations hidden", class = "coursekata_squares_hidden"
  )
})

test_that("mapped and staged linewidths survive rendering without a border cap", {
  d <- data.frame(x = c(1, 1, 2, 2), g = c("a", "b", "a", "b"))
  for (coord in list(ggplot2::coord_cartesian(), ggplot2::coord_flip(),
                    ggplot2::coord_transform(y = "sqrt"))) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, linewidth = g)) +
      geom_squareplot() + ggplot2::scale_linewidth_manual(values = c(a = .2, b = 1)) + coord
    built <- ggplot2::layer_data(p)
    expect_false(any(built$fit_border))
    expect_equal(squareplot_render_panel(p)$gp$lwd, built$linewidth * ggplot2::.pt)
  }
  staged <- ggplot2::ggplot(d, ggplot2::aes(x)) +
    geom_squareplot(ggplot2::aes(linewidth = ggplot2::after_scale(.3)))
  expect_false(any(ggplot2::layer_data(staged)$fit_border))
  expect_equal(squareplot_render_panel(staged)$gp$lwd, rep(.3 * ggplot2::.pt, nrow(d)))
})

test_that("non-whole counts cannot be silently truncated", {
  for (count in c(-1, .6, Inf, NA_real_)) {
    data <- data.frame(x = 1, width = 1, count = count, PANEL = 1, group = 1)
    expect_error(GeomSquareplot$setup_data(data, list()), class = "coursekata_squareplot_count")
  }
  d <- data.frame(x = c(1, 1, 2), w = c(.6, .6, .6))
  for (x in list(d$x, factor(d$x))) {
    d$x <- x
    for (bars in c("none", "outline", "solid")) {
      expect_error(
        ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, weight = w)) +
          geom_squareplot(bars = bars)), "whole counts"
      )
    }
  }
  d$w <- c(2, 0, 3)
  expect_equal(nrow(ggplot2::layer_data(
    ggplot2::ggplot(d, ggplot2::aes(x, weight = w)) + geom_squareplot()
  )), 5)
  near <- data.frame(x = 1, width = 1, count = 2 - 1e-10, PANEL = 1, group = 1)
  expect_equal(nrow(GeomSquareplot$setup_data(near, list())), 2)
})

test_that("classification and drawing use the same inherited layer rows", {
  d <- data.frame(x = factor(c("a", "a", "b"), levels = c("a", "b", "c")))
  base <- ggplot2::ggplot() + ggplot2::geom_blank(data = d, ggplot2::aes(x))
  for (constructor in list(geom_squareplot, stat_squareplot)) {
    result <- base + constructor()
    expect_identical(result$layers[[2]]$stat, StatSquareplotCount)
    expect_equal(nrow(ggplot2::layer_data(result, 2)), 3)
    expect_length(squareplot_render_panel(result)$x, 3)
  }
  expect_equal(nrow(ggplot2::layer_data(gf_squareplot(base), 2)), 3)
  expect_identical(base$layers[[1]]$data, d)
  override <- data.frame(x = factor(c("a", "a", "b", "c")))
  expect_equal(nrow(ggplot2::layer_data(base + geom_squareplot(
    data = override, mapping = ggplot2::aes(x), inherit.aes = FALSE
  ), 2)), 4)
})

test_that("gf inherits the actionable x-only diagnostic", {
  d <- data.frame(x = 1:3, y = 2:4)
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point()
  expect_error(gf_squareplot(base), "`gf_squareplot\\(\\)` draws the distribution mapped to x")
  expect_error(gf_squareplot(base, ~x), "inherit = FALSE")
  expect_equal(nrow(ggplot2::layer_data(gf_squareplot(base, ~x, inherit = FALSE), 2)), 3)
})

test_that("classification probes leave the random stream unchanged", {
  d <- data.frame(x = 1:10)
  set.seed(782)
  before <- .Random.seed
  invisible(gf_squareplot(~sample(x), data = d))
  expect_identical(.Random.seed, before)
})

test_that("one squareplot layer can be reused across different x types", {
  layer <- geom_squareplot()
  continuous <- ggplot2::ggplot(data.frame(x = c(1, 1, 2)), ggplot2::aes(x)) + layer
  discrete <- ggplot2::ggplot(data.frame(x = factor(c("a", "b"))), ggplot2::aes(x)) + layer
  expect_identical(continuous$layers[[1]]$stat, StatSquareplot)
  expect_identical(discrete$layers[[1]]$stat, StatSquareplotCount)
  expect_equal(nrow(ggplot2::layer_data(continuous)), 3)
  expect_equal(nrow(ggplot2::layer_data(discrete)), 2)
  expect_identical(layer$stat, StatSquareplot)
  expect_null(layer$mapping)
})

test_that("ordinary plot data and mappings remain inherited after addition", {
  d <- data.frame(x = c(1, 1, 2), z = c(4, 5, 5))
  p <- ggplot2::ggplot(d, ggplot2::aes(x)) + geom_squareplot()
  expect_true(ggplot2::is_waiver(p$layers[[1]]$data))
  expect_null(p$layers[[1]]$mapping)
  replaced <- replace_plot_data(p, data.frame(x = 1:5, z = 6:10))
  expect_equal(nrow(ggplot2::layer_data(replaced)), 5)
  changed <- ggplot2::layer_data(p + ggplot2::aes(x = z))
  expect_equal(sort(changed$x), d$z)
  expect_equal(nrow(ggplot2::layer_data(p)), 3)
})

test_that("constant positional mappings work without a data frame", {
  p <- ggplot2::ggplot() + geom_squareplot(ggplot2::aes(x = c(1, 1, 2)))
  expect_equal(nrow(ggplot2::layer_data(p)), 3)
})

test_that("plot-level staged linewidths retain fitting in either addition order", {
  d <- data.frame(x = rep(1, 1000))
  base <- ggplot2::ggplot(d, ggplot2::aes(x))
  late <- base + geom_squareplot() + ggplot2::aes(linewidth = ggplot2::after_scale(.3))
  early <- base + ggplot2::aes(linewidth = ggplot2::after_scale(.3)) + geom_squareplot()
  local <- base + geom_squareplot(ggplot2::aes(linewidth = ggplot2::after_scale(.3)))
  expect_equal(unique(ggplot2::layer_data(late)$linewidth), .3)
  expect_true(all(ggplot2::layer_data(late)$fit_border))
  expect_identical(ggplot2::layer_data(early), ggplot2::layer_data(late))
  expect_false(any(ggplot2::layer_data(local)$fit_border))
  expect_lt(max(squareplot_render_panel(late)$gp$lwd), .3 * ggplot2::.pt)
  expect_warning(
    local_drawn <- squareplot_render_panel(local), class = "coursekata_squares_hidden"
  )
  expect_equal(local_drawn$gp$lwd, rep(.3 * ggplot2::.pt, 1000))
})

test_that("removing a plot-level linewidth mapping restores automatic fitting", {
  d <- data.frame(x = rep(1, 1000), width = rep(.3, 1000))
  for (mapping in list(ggplot2::aes(linewidth = width),
                       ggplot2::aes(linewidth = ggplot2::after_scale(.3)))) {
    plot <- ggplot2::ggplot(d, ggplot2::aes(x)) + mapping + geom_squareplot()
    removed <- plot + ggplot2::aes(linewidth = NULL)
    baseline <- ggplot2::ggplot(d, ggplot2::aes(x)) + geom_squareplot()
    expect_true(all(ggplot2::layer_data(removed)$fit_border))
    expect_equal(squareplot_render_panel(removed)$gp$lwd, squareplot_render_panel(baseline)$gp$lwd)
  }
})
