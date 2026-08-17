built_of <- function(p) ggplot2::ggplot_build(p)
y_breaks <- function(p) {
  b <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_breaks()
  b[!is.na(b)]
}
x_breaks <- function(p) {
  b <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_breaks()
  b[!is.na(b)]
}
x_labels <- function(p) ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
# ggplot2 3.5.2 fills p$labels eagerly and ggplot2 4 leaves it empty until build,
# so a default label is only portable when it is read from the built plot
label_of <- function(p, aes) as.character(ggplot2::ggplot_build(p)$plot$labels[[aes]])
rect_grobs <- function(p) {
  grDevices::pdf(NULL, width = 7, height = 4.5)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(p)
  grid::grid.force()
  drawn <- grid::grid.get("geom_rect", grep = TRUE, global = TRUE)
  if (grid::is.grob(drawn)) list(drawn) else drawn
}
squares <- data.frame(x = c(1, 1, 2, 2, 2, 3, 3, 4, 5, 5), g = rep(c("a", "b"), 5))

squareplot_bins <- function(p) {
  built <- built_of(p)$data[[1]]
  built <- built[!duplicated(built$xmin), ]
  built <- built[order(built$xmin), c("xmin", "xmax", "count")]
  rownames(built) <- NULL
  built
}
histogram_bins <- function(p) {
  built <- built_of(p)$data[[1]]
  built <- built[built$count > 0, ]
  built <- built[order(built$xmin), c("xmin", "xmax", "count")]
  rownames(built) <- NULL
  built
}

test_that("the plot carries the caller's data, so it composes like any other gf_ layer", {
  p <- gf_squareplot(~x, data = squares)
  expect_s3_class(p, "gf_ggplot")
  expect_equal(nrow(p$data), 10)
  expect_equal(rlang::as_name(p$mapping$x), "x")
  expect_equal(length(p$layers), 1)
})

test_that("the layer_factory contract holds: labels, facets, piping, mapped aesthetics", {
  p <- gf_squareplot(~x, data = squares, title = "TT", xlab = "XX", ylab = "YY")
  expect_equal(p$labels$title, "TT")
  expect_equal(p$labels$x, "XX")
  expect_equal(p$labels$y, "YY")

  faceted <- gf_squareplot(~ x | g, data = squares)
  expect_s3_class(faceted$facet, "FacetWrap")
  expect_equal(nrow(built_of(faceted)$layout$layout), 2)

  piped <- squares %>% gf_squareplot(~x)
  expect_equal(nrow(built_of(piped)$data[[1]]), 10)

  coloured <- gf_squareplot(~x, data = squares, fill = ~g)
  expect_equal(length(unique(built_of(coloured)$data[[1]]$fill)), 2)
  expect_equal(length(built_of(coloured)$plot$guides$guides), 1)

  styled <- gf_squareplot(~x, data = squares, fill = "coral", alpha = 0.35)
  expect_equal(unique(built_of(styled)$data[[1]]$fill), "coral")
  expect_equal(unique(built_of(styled)$data[[1]]$alpha), 0.35)

  onto <- gf_histogram(~x, data = squares, bins = 5) %>% gf_squareplot()
  expect_equal(length(onto$layers), 2)
  expect_equal(nrow(built_of(onto)$data[[2]]), 10)
})

test_that("the plot can be read by the functions that read a plot", {
  p <- gf_squareplot(~Thumb, data = Fingers)
  ruled <- p %>% gf_sd_ruler()
  expect_equal(nrow(built_of(ruled)$data[[2]]), 1)
  modelled <- p %>% gf_model(lm(Thumb ~ NULL, data = Fingers))
  expect_equal(length(modelled$layers), 2)
  expect_equal(nrow(built_of(p + ggplot2::facet_wrap(~Sex))$layout$layout), 2)
})

test_that("the y axis defaults to a count and the x axis to the expression plotted", {
  p <- gf_squareplot(~x, data = squares)
  expect_equal(label_of(p, "y"), "count")
  expect_equal(label_of(gf_squareplot(~ log(x), data = squares), "x"), "log(x)")
})

test_that("caller-owned scales win, and incompatible count axes are refused", {
  base <- gf_histogram(~x, data = squares, bins = 5) +
    ggplot2::scale_x_continuous(breaks = c(1, 3, 5)) +
    ggplot2::scale_y_continuous(breaks = c(0, 2, 4), limits = c(0, 6))
  expect_no_warning(composed <- base %>% gf_squareplot())
  expect_equal(composed$scales$get_scales("x")$breaks, c(1, 3, 5))
  expect_equal(composed$scales$get_scales("y")$breaks, c(0, 2, 4))
  expect_equal(composed$scales$get_scales("y")$limits, c(0, 6))
  expect_equal(composed$scales$get_scales("y")$get_transformation()$name, "identity")

  factor_base <- ggplot2::ggplot(
    data.frame(x = factor(c("a", "c"), levels = c("a", "b", "c"))),
    ggplot2::aes(x)
  ) + ggplot2::scale_x_discrete(limits = c("c", "b", "a"), drop = TRUE)
  factor_plot <- factor_base %>% gf_squareplot()
  expect_equal(factor_plot$scales$get_scales("x")$limits, c("c", "b", "a"))
  expect_true(factor_plot$scales$get_scales("x")$drop)

  # a scale transform is applied before the squares are built, so it is
  # redirected to the coord spelling rather than refused as impossible
  expect_error(
    (gf_histogram(~x, data = squares) + ggplot2::scale_y_sqrt()) %>% gf_squareplot(),
    'coord_transform\\(y = "sqrt"\\)'
  )
  expect_error(
    ggplot2::ggplot_build(gf_squareplot(~x, data = squares) + ggplot2::scale_y_sqrt()),
    'coord_transform\\(y = "sqrt"\\)'
  )
  # a discrete y keeps its refusal, and says why rather than naming a scale type
  expect_error(
    ggplot2::ggplot_build(gf_squareplot(~x, data = squares) + ggplot2::scale_y_discrete()),
    "no count to be one of"
  )
})

test_that("the stat bins the values the scale actually draws", {
  decades <- data.frame(x = c(1, 10, 100, 1000))
  p <- gf_squareplot(~x, data = decades) + ggplot2::scale_x_log10()
  built <- built_of(p)
  expect_equal(sort(unique(built$data[[1]]$xmin)), c(-0.5, 0.5, 1.5, 2.5))
  expect_equal(nrow(built$data[[1]]), 4)
  expect_lt(diff(built$layout$panel_params[[1]]$x.range), 5)

  h <- gf_histogram(~x, data = decades, binwidth = 1) + ggplot2::scale_x_log10()
  expect_equal(squareplot_bins(p), histogram_bins(h))
})

test_that("the count axis is whole numbers, decided by the data maximum", {
  ten <- gf_squareplot(~x, data = data.frame(x = rep(1, 10)))
  eleven <- gf_squareplot(~x, data = data.frame(x = rep(1, 11)))
  expect_equal(y_breaks(ten), 0:10)
  expect_equal(y_breaks(eleven), seq(0, 10, by = 2))
  expect_equal(y_breaks(gf_squareplot(~x, data = data.frame(x = rep(1, 20)))), seq(0, 20, by = 2))
  expect_equal(y_breaks(gf_squareplot(~x, data = data.frame(x = rep(1, 21)))), seq(0, 20, by = 5))
})

test_that("expand_limits sets the floor mincount used to", {
  small <- data.frame(x = c(1, 1, 2, 2, 2, 3))
  p <- gf_squareplot(~x, data = small) + ggplot2::expand_limits(y = 10)
  expect_equal(y_breaks(p), 0:10)
})

test_that("gf_lims sets the x range xrange used to", {
  # silence: ggplot2 announces a replaced scale with a message, so the squareplot
  # having claimed the x axis this replaces would show up here and nowhere else
  expect_no_message(p <- gf_squareplot(~x, data = squares) %>% gf_lims(x = c(-2, 12)))
  expect_equal(p$scales$get_scales("x")$limits, c(-2, 12))
  # the panel is the limits plus ggplot2's own 5% expansion, as for any other layer
  expect_equal(built_of(p)$layout$panel_params[[1]]$x.range, c(-2.7, 12.7))
})

test_that("scale_x_continuous sets the x breaks xbreaks used to", {
  expect_no_message(
    p <- gf_squareplot(~x, data = squares) +
      ggplot2::scale_x_continuous(breaks = c(1, 3, 5))
  )
  expect_equal(x_breaks(p), c(1, 3, 5))
})

test_that("an overlay drawn above the counts does not relabel the count axis", {
  set.seed(42)
  shuffled <- data.frame(b1 = replicate(10, {
    b1(lm(base::sample(TipExperiment$Tip) ~ Condition, data = TipExperiment))
  }))
  framed <- gf_squareplot(~b1, data = shuffled, binwidth = 2) %>%
    gf_lims(x = c(-30, 30)) %>%
    gf_refine(ggplot2::expand_limits(y = 10)) %>%
    show_mean() %>%
    show_dgp()
  # the band sits from 11.2 up; a tick at 12 would claim a count that is not there
  expect_equal(y_breaks(framed), 0:10)
  expect_equal(built_of(framed)$layout$panel_params[[1]]$y.range, c(-0.7, 14.7))
})

test_that("a factor level nobody landed in still holds its place on the axis", {
  top <- data.frame(x = factor(c(1, 1, 3, 3), levels = 1:5))
  expect_equal(x_labels(gf_squareplot(~x, data = top)), as.character(1:5))
  expect_equal(
    sort(unique(as.numeric(built_of(gf_squareplot(~x, data = top))$data[[1]]$x))),
    c(1, 3)
  )

  # an unobserved level BETWEEN two observed ones is the case the scale must carry:
  # dropping it does not merely lose a tick, it moves the columns that are drawn
  interior <- data.frame(x = factor(c(1, 1, 5, 5), levels = 1:5))
  expect_equal(x_labels(gf_squareplot(~x, data = interior)), as.character(1:5))
  expect_equal(
    sort(unique(as.numeric(built_of(gf_squareplot(~x, data = interior))$data[[1]]$x))),
    c(1, 5)
  )
})

test_that("a discrete x counts each level instead of binning its position", {
  d <- data.frame(x = factor(c("a", "a", "b", "c", "c", "c", "d", "e"), levels = letters[1:5]))
  p <- gf_squareplot(~x, data = d)
  built <- built_of(p)$data[[1]]
  expect_equal(x_labels(p), letters[1:5])

  columns <- unique(built[, c("x", "xmin", "xmax")])
  columns <- columns[order(columns$x), ]
  expect_equal(as.numeric(columns$x), seq_along(letters[1:5]))
  expect_equal(columns$xmin, columns$x - 0.45)
  expect_equal(columns$xmax, columns$x + 0.45)

  expect_equal(nrow(built), 8)
  expect_equal(as.numeric(table(built$x)), c(2, 1, 3, 1, 1))
})

test_that("a discrete x draws the columns gf_bar() draws, level for level", {
  check_parity <- function(d) {
    squares <- built_of(gf_squareplot(~x, data = d))$data[[1]]
    bars <- built_of(gf_bar(~x, data = d))$data[[1]]

    sq_cols <- unique(squares[, c("x", "xmin", "xmax")])
    sq_cols <- sq_cols[order(sq_cols$x), ]
    rownames(sq_cols) <- NULL
    bar_cols <- bars[order(bars$x), c("x", "xmin", "xmax")]
    rownames(bar_cols) <- NULL
    expect_identical(sq_cols, bar_cols)
    expect_identical(as.numeric(table(squares$x)), as.numeric(bars$count))
  }

  check_parity(data.frame(x = factor(c("a", "a", "b", "c", "c", "c", "d", "e"), levels = letters[1:5])))
  check_parity(data.frame(x = c("a", "a", "b", "c")))
  check_parity(data.frame(x = c(TRUE, TRUE, FALSE)))
  check_parity(data.frame(x = factor(seq_len(60))))
})

test_that("a factor the plot already carries is counted, and keeps its unused levels", {
  gaps <- data.frame(x = factor(c(1, 1, 5, 5), levels = 1:5))
  p <- ggplot2::ggplot(gaps, ggplot2::aes(.data$x)) %>% gf_squareplot()
  built <- built_of(p)
  expect_equal(unique(built$data[[1]]$x), c(1, 5))
  expect_equal(x_labels(p), as.character(1:5))
})

test_that("a binning argument that cannot reach a counted x says so", {
  d <- data.frame(x = factor(c("a", "a", "b")))
  expect_warning(gf_squareplot(~x, data = d, binwidth = 2), "counted, not binned")
  expect_warning(gf_squareplot(~x, data = d, boundary = 0), "`boundary`")
  expect_warning(gf_squareplot(~x, data = d, bins = 3, closed = "left"), "`bins`.*`closed`")

  expect_no_warning(gf_squareplot(~x, data = d))
  expect_no_warning(gf_squareplot(~x, data = squares, binwidth = 2))
})

test_that("a solid bars on a discrete x keeps its level's group composition", {
  d <- data.frame(
    x = factor(c("a", "a", "a", "b", "b", "b")),
    g = c("x", "x", "y", "x", "y", "y")
  )
  gs <- rect_grobs(gf_squareplot(~x, data = d, bars = "solid", fill = ~g))
  expect_length(gs, 1)
  rects <- gs[[1]]
  expect_length(rects$x, 4)

  fills <- rects$gp$fill
  expect_length(unique(fills), 2)
  expect_equal(fills[1:2], fills[3:4])

  height <- as.numeric(rects$height)
  expect_equal(height / min(height), c(2, 1, 1, 2))

  xs <- as.numeric(rects$x)
  expect_equal(xs[1], xs[2])
  expect_equal(xs[3], xs[4])
  expect_lt(xs[1], xs[3])
})

test_that("every bar mode is the same single layer", {
  for (mode in c("none", "outline", "solid")) {
    p <- gf_squareplot(~x, data = squares, bars = mode)
    expect_equal(length(p$layers), 1, label = paste("layers for bars =", mode))
    expect_equal(p$layers[[1]]$geom_params$bars, mode)
  }
})

test_that("the bar mode is read and written under the one name the geom documents", {
  # `$` partial-matches on read but not on assignment, so `params$bar` read
  # `bars` by accident and then wrote a second element that nothing draws from.
  # The validation happened to see the right value; the parameter it left behind
  # was a name the geom does not have
  withr::local_options(warnPartialMatchDollar = TRUE)

  params <- GeomSquareplot$setup_params(NULL, list(bars = "outline", na.rm = TRUE))
  expect_setequal(names(params), c("bars", "na.rm"))
  expect_equal(params$bars, "outline")

  expect_no_warning(ggplot2::ggplot_build(gf_squareplot(~x, data = squares, bars = "outline")))
  # setup_params() runs at build time, which is where ggplot2 validates a geom parameter
  expect_error(
    ggplot2::ggplot_build(gf_squareplot(~x, data = squares, bars = "bogus")),
    "must be one of"
  )
})

test_that("color is the separator between squares, and bar_colour is the bar's own", {
  d <- data.frame(x = c(1, 1, 2), g = c("a", "b", "a"))

  # unmapped, the separators default to white and the bar keeps the geom's grey20
  plain <- rect_grobs(gf_squareplot(~x, data = d, bars = "outline"))
  expect_length(plain, 2)
  expect_equal(unique(plain[[1]]$gp$col), "white")
  expect_equal(unique(plain[[2]]$gp$col), "grey20")
  expect_equal(unique(built_of(gf_squareplot(~x, data = d))$data[[1]]$fill), "#7fcecc")

  # color reaches the separators, not the bar
  red <- rect_grobs(gf_squareplot(~x, data = d, bars = "outline", color = "red"))
  expect_equal(unique(red[[1]]$gp$col), "red")
  expect_equal(unique(red[[2]]$gp$col), "grey20")

  # and bar_colour reaches the bar, not the separators
  navy <- rect_grobs(gf_squareplot(~x, data = d, bars = "outline", bar_colour = "navy"))
  expect_equal(unique(navy[[1]]$gp$col), "white")
  expect_equal(unique(navy[[2]]$gp$col), "navy")

  # "black" used to be laundered into grey20, so the one argument aimed at the
  # bar could not ask for black
  black <- rect_grobs(gf_squareplot(~x, data = d, bars = "outline", bar_colour = "black"))
  expect_equal(unique(black[[2]]$gp$col), "black")
})

test_that("a mapped colour varies the separators across groups", {
  d <- data.frame(x = c(1, 1, 2, 2), g = c("a", "b", "a", "b"))
  built <- built_of(gf_squareplot(~x, data = d, color = ~g, binwidth = 1))$data[[1]]
  expect_length(unique(built$colour), 2)
  # it is an aesthetic, so it earns a legend
  expect_equal(length(built_of(gf_squareplot(~x, data = d, color = ~g, binwidth = 1))$plot$guides$guides), 1)
})

test_that("a bin of more than 75 observations still draws one square per observation", {
  # this is what `auto_subdivide = TRUE` used to opt into, and it is why the argument
  # could be dropped rather than replaced: unit-tall squares at any size, unasked
  set.seed(24)
  big <- data.frame(x = stats::rnorm(2000, 50, 10))
  built <- built_of(gf_squareplot(~x, data = big))$data[[1]]
  expect_equal(nrow(built), 2000)
  expect_equal(unique(round(built$ymax - built$ymin, 6)), 1)
  expect_gt(max(built$ymax), 75)
})

test_that("the histogram's own binning arguments reach the stat", {
  p <- gf_squareplot(~x, data = squares, binwidth = 2, boundary = 0)
  expect_equal(sort(unique(built_of(p)$data[[1]]$xmin)), c(0, 2, 4))

  three <- gf_squareplot(~x, data = squares, bins = 3)
  expect_equal(nrow(squareplot_bins(three)), 3)

  breaks <- c(0, 3, 6)
  by_breaks <- gf_squareplot(~x, data = squares, breaks = breaks)
  hist_by_breaks <- gf_histogram(~x, data = squares, breaks = breaks)
  expect_equal(squareplot_bins(by_breaks), histogram_bins(hist_by_breaks))
})

test_that("the signature is a histogram's binning vocabulary, and every name in it is read", {
  args <- names(formals(gf_squareplot))
  expect_equal(args, c(
    "object", "gformula", "data", "...", "binwidth", "bins", "center", "boundary",
    "closed", "breaks", "bars", "na.rm", "xlab", "ylab", "title", "subtitle", "caption",
    "geom", "stat", "position", "show.legend", "show.help", "inherit", "environment"
  ))

  expect_equal(
    intersect(args, StatSquareplot$parameters()),
    c("binwidth", "bins", "center", "boundary", "closed", "breaks")
  )

  # the names that left the signature really left it
  expect_equal(
    intersect(args, c("xrange", "xbreaks", "mincount", "show_mean", "show_dgp", "auto_subdivide")),
    character(0)
  )

  sheet <- paste(testthat::capture_messages(gf_squareplot()), collapse = " ")
  sheet <- gsub("\\s+", " ", sheet)
  expect_match(
    sheet,
    "binwidth = NULL, bins = NULL, center = NULL, boundary = NULL, closed = NULL, breaks = NULL",
    fixed = TRUE
  )
})

test_that("a squareplot's bins are a histogram's bins, argument for argument", {
  dice <- data.frame(x = c(1, 1, 2, 2, 2, 3, 3, 4, 5, 5))
  boundary_data <- data.frame(x = c(0, 1, 2, 3, 4, 5, 6))

  cases <- list(
    list(data = Fingers, x = "Thumb", args = list(binwidth = 1.7)),
    list(data = dice, x = "x", args = list(binwidth = 1)),
    list(data = Fingers, x = "Thumb", args = list(binwidth = 5)),
    list(data = Fingers, x = "Thumb", args = list(bins = 8)),
    list(data = Fingers, x = "Thumb", args = list(binwidth = 5, boundary = 0)),
    list(data = Fingers, x = "Thumb", args = list(binwidth = 5, center = 50)),
    list(data = Fingers, x = "Thumb", args = list(breaks = c(30, 50, 70, 90))),
    list(data = boundary_data, x = "x", args = list(binwidth = 2, boundary = 0, closed = "left")),
    list(data = boundary_data, x = "x", args = list(binwidth = 2, boundary = 0, closed = "right")),
    list(data = dice, x = "x", args = list(binwidth = 1, boundary = 0.5))
  )

  for (case in cases) {
    frm <- stats::as.formula(paste0("~", case$x))
    sq <- do.call(gf_squareplot, c(list(frm, data = case$data), case$args))
    hi <- do.call(gf_histogram, c(list(frm, data = case$data), case$args))
    expect_equal(
      squareplot_bins(sq), histogram_bins(hi),
      label = paste(names(case$args), collapse = ",")
    )
  }
})

test_that("the maximum lands in the last bin instead of one of its own past the data", {
  p <- gf_squareplot(~Thumb, data = Fingers, binwidth = 5, boundary = 0)
  bins <- squareplot_bins(p)
  expect_equal(max(bins$xmax), max(Fingers$Thumb))
  expect_equal(sum(bins$count), sum(!is.na(Fingers$Thumb)))
})

test_that("a name that left the signature is discarded, not given a rule of its own", {
  # xrange, xbreaks, mincount, show_mean, show_dgp and auto_subdivide were all
  # arguments once. They are names the layer does not recognise now, and
  # ggformula discards one of those without a word -- the same as any
  # misspelling, and the same as every other gf_ function. The replacements are
  # documented; they are not enforced here
  expect_no_error(gf_squareplot(~x, data = squares, xrange = c(0, 6)))
  expect_no_error(gf_squareplot(~x, data = squares, mincount = 10))
  expect_no_error(gf_squareplot(~x, data = squares, auto_subdivide = TRUE))
  expect_no_error(gf_point(x ~ x, data = squares, mincount = 10))
})

test_that("the input guards that survive the move still fire", {
  d <- data.frame(x = c(1, 2, 2, NA))
  expect_error(gf_squareplot(~x, data = d, na.rm = FALSE), "na.rm")
  expect_equal(nrow(built_of(gf_squareplot(~x, data = d))$data[[1]]), 3)
  expect_error(gf_squareplot(x ~ g, data = squares), "one-sided")
  expect_error(gf_squareplot(squares$x), "data = ")
})

test_that("a bare call prints its own help instead of drawing", {
  expect_message(expect_null(gf_squareplot()), "a formula with shape")
})

test_that("a large distribution still draws its fitted border", {
  set.seed(24)
  p <- gf_squareplot(~x, data = data.frame(x = rnorm(2000, 50, 10)))
  drawn <- rect_grobs(p)[[1]]
  expect_equal(length(drawn$x), 2000)
  expect_length(drawn$gp$lwd, 2000)
  expect_lt(max(drawn$gp$lwd), 0.5 * ggplot2::.pt)
})

test_that("a coord transform distorts the squares instead of being refused", {
  # the distortion IS the picture: a square is one count wherever it sits, so a
  # non-linear count axis draws the higher ones shorter, which is what shows a
  # reader that the unit changes as the scale climbs
  d <- data.frame(x = c(rep(1, 16), rep(2, 4)))
  p <- gf_squareplot(~x, data = d, binwidth = 1) %>%
    gf_refine(ggplot2::coord_transform(y = "sqrt"))

  expect_no_error(built <- ggplot2::ggplot_build(p))
  # every square still spans exactly one count before the coord draws it
  drawn <- built$data[[1]]
  expect_true(all(drawn$ymax - drawn$ymin == 1))
  # and the axis still counts
  labels <- built$layout$panel_params[[1]]$y$get_labels()
  expect_true(all(grepl("^[0-9]+$", stats::na.omit(labels))))

  heights <- coursekata:::squareplot_drawn_heights(
    drawn, built$layout$panel_params[[1]], built$layout$coord
  )
  tallest <- heights[which.min(drawn$ymin)]
  shortest <- heights[which.max(drawn$ymin)]
  expect_gt(tallest, shortest * 2)
})

test_that("an identity coord leaves every square the same height", {
  # the mirror of the test above: the same code path must not distort a plot
  # nobody asked to distort
  d <- data.frame(x = c(rep(1, 16), rep(2, 4)))
  built <- ggplot2::ggplot_build(gf_squareplot(~x, data = d, binwidth = 1))
  heights <- coursekata:::squareplot_drawn_heights(
    built$data[[1]], built$layout$panel_params[[1]], built$layout$coord
  )

  expect_length(unique(round(heights, 10)), 1)
})

test_that("a border is fitted to the square it borders, not to the first one", {
  # under a coord transform the top squares are drawn a fraction of the height
  # of the bottom ones, so one stroke for the layer would swallow them
  set.seed(24)
  d <- data.frame(x = rnorm(400, 50, 10))
  drawn <- rect_grobs(
    gf_squareplot(~x, data = d) %>% gf_refine(ggplot2::coord_transform(y = "sqrt"))
  )[[1]]

  expect_gt(length(unique(round(drawn$gp$lwd, 3))), 1)
  expect_lt(min(drawn$gp$lwd), max(drawn$gp$lwd))
})

test_that("gf_squareplot snapshot", {
  skip_if_not_installed("vdiffr")
  int_data <- data.frame(x = c(1, 1, 2, 2, 2, 3, 3, 4, 5, 5))
  gf_squareplot(~x, data = int_data) %>%
    expect_doppelganger("gf_squareplot-basic-int")
})
