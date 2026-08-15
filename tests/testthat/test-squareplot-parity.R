# A squareplot is a histogram whose bars are drawn as the observations that make
# them up, so for the same arguments it must bin to the same edges and the same
# counts. These tests are the equality, not a resemblance.

# One row per occupied bin: its group, its edges and its count. A bin nobody landed
# in has no squares to count, so it is dropped from both sides here; the whole grid,
# empty bins included, is compared at the stat further down.
bin_frame <- function(group, xmin, xmax, count) {
  out <- data.frame(
    group = as.integer(group), xmin = as.numeric(xmin),
    xmax = as.numeric(xmax), count = as.numeric(count)
  )
  out <- out[out$count > 0, , drop = FALSE]
  out <- out[order(out$group, out$xmin), , drop = FALSE]
  row.names(out) <- NULL
  out
}

# a histogram layer already has one row per bin
hist_bins <- function(p, i = 1) {
  d <- ggplot2::layer_data(p, i)
  bin_frame(d$group, d$xmin, d$xmax, d$count)
}

# a squareplot layer has one row per square, so count them back into their bins
square_bins <- function(p, i = 1) {
  d <- ggplot2::layer_data(p, i)
  key <- interaction(d$group, d$xmin, drop = TRUE, lex.order = TRUE)
  bin_frame(
    tapply(d$group, key, `[`, 1L),
    tapply(d$xmin, key, `[`, 1L),
    tapply(d$xmax, key, `[`, 1L),
    tapply(d$xmin, key, length)
  )
}

dice <- data.frame(roll = c(1L, 1L, 2L, 3L, 3L, 3L, 4L, 5L, 6L, 6L))
# every value sits on a bin edge at binwidth 2.5 from 0, so `closed` always matters
on_edge <- data.frame(x = c(0, 2.5, 5, 5, 7.5, 10))

parity_cases <- list(
  # the width the old grid rule and stat_bin place differently
  "binwidth" = list(quote(~Thumb), Fingers, list(binwidth = 5)),
  # a count of bins, which only the stat can turn into a width
  "bins" = list(quote(~Thumb), Fingers, list(bins = 8)),
  # pins the grid to multiples of 5, where the largest Thumb sits on an edge
  "boundary" = list(quote(~Thumb), Fingers, list(binwidth = 5, boundary = 0)),
  # the other way to name the same grid
  "center" = list(quote(~Thumb), Fingers, list(binwidth = 5, center = 40)),
  "closed left" = list(quote(~Thumb), Fingers, list(binwidth = 5, boundary = 0, closed = "left")),
  "closed right" = list(quote(~Thumb), Fingers, list(binwidth = 5, boundary = 0, closed = "right")),
  # unequal widths, which no binwidth of ours could reproduce
  "breaks" = list(quote(~Thumb), Fingers, list(breaks = c(30, 50, 60, 70, 95))),
  # the integer fix the histogram would use too
  "integer data" = list(quote(~roll), dice, list(binwidth = 1, boundary = 0.5)),
  "values on an edge" = list(quote(~x), on_edge, list(binwidth = 2.5, boundary = 0)),
  "values on an edge, left" =
    list(quote(~x), on_edge, list(binwidth = 2.5, boundary = 0, closed = "left")),
  "values on an edge, right" =
    list(quote(~x), on_edge, list(binwidth = 2.5, boundary = 0, closed = "right")),
  # a mapped fill bins within each group, in both plots
  "a mapped fill" = list(quote(~Thumb), Fingers, list(fill = ~Sex, binwidth = 5, boundary = 0))
)

for (name in names(parity_cases)) {
  case <- parity_cases[[name]]
  test_that(paste0("a squareplot bins exactly as a histogram does: ", name), {
    args <- c(list(case[[1]], data = case[[2]]), case[[3]])
    expect_identical(
      square_bins(do.call(gf_squareplot, args)),
      hist_bins(do.call(gf_histogram, args))
    )
  })
}

test_that("closed is not decoration: it moves the values sitting on the edges", {
  left <- square_bins(gf_squareplot(~x, data = on_edge, binwidth = 2.5, boundary = 0,
                                    closed = "left"))
  right <- square_bins(gf_squareplot(~x, data = on_edge, binwidth = 2.5, boundary = 0,
                                     closed = "right"))
  expect_identical(left$count, c(1, 1, 2, 2))
  expect_identical(right$count, c(2, 2, 1, 1))
})

test_that("squares laid over bars in one plot land in the same bins", {
  overlaid <- gf_histogram(~Thumb, data = Fingers, bins = 8) %>% gf_squareplot(bins = 8)
  expect_equal(length(overlaid$layers), 2)
  expect_identical(square_bins(overlaid, 2), hist_bins(overlaid, 1))
})

test_that("the default binwidth is a histogram's own binning at the width we chose", {
  expect_identical(
    square_bins(gf_squareplot(~Thumb, data = Fingers)),
    hist_bins(gf_histogram(~Thumb, data = Fingers,
                           binwidth = squareplot_binwidth(Fingers$Thumb)))
  )
  expect_identical(
    square_bins(gf_squareplot(~roll, data = dice)),
    hist_bins(gf_histogram(~roll, data = dice, binwidth = 1))
  )
})

test_that("the squareplot chooses its own width, so the histogram's advice never fires", {
  expect_message(ggplot2::ggplot_build(gf_histogram(~roll, data = dice)), "Pick better value")
  expect_no_message(
    ggplot2::ggplot_build(gf_squareplot(~roll, data = dice)),
    message = "Pick better value"
  )
})

# the geom expands a bin into squares, so an empty bin leaves no trace in a drawn
# squareplot; the grid it was drawn on is only visible at the stat
stat_grid <- function(stat, x, ...) {
  p <- ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = .data$x)) +
    ggplot2::layer(stat = stat, geom = "blank", position = "identity", params = list(...))
  ggplot2::layer_data(p, 1)
}

grid_cases <- list(
  # (80, 85] holds nothing, and the axis has to keep the gap
  "an interior empty bin" = list(binwidth = 5, boundary = 0),
  # pad's empty flanking bins are invisible in the squares
  "pad" = list(binwidth = 5, boundary = 0, pad = TRUE),
  "closed left" = list(binwidth = 5, boundary = 0, closed = "left"),
  "bins" = list(bins = 8)
)

for (name in names(grid_cases)) {
  params <- grid_cases[[name]]
  test_that(paste0("the stat produces the histogram's whole grid, empty bins and all: ", name), {
    expect_identical(
      do.call(stat_grid, c(list(StatSquareplot, Fingers$Thumb), params)),
      do.call(stat_grid, c(list("bin", Fingers$Thumb), params))
    )
  })
}

test_that("the stat has no binning code of its own", {
  # the y-axis guard and the default width are ours; where a bin starts and what
  # falls into it is StatBin's, and there is nowhere here for it to be re-decided
  expect_equal(intersect(ls(StatSquareplot), c("compute_group", "compute_panel")), character(0))
  expect_identical(StatSquareplot$parameters(), ggplot2::StatBin$parameters())
})

test_that("the squareplot takes the histogram's binning vocabulary, argument for argument", {
  # pad is deliberately not one of these: the squares have nothing to draw for an
  # empty flanking bin, so it stays reachable only through `...`, asserted below
  expect_true(all(
    c("bins", "binwidth", "center", "boundary", "breaks", "closed") %in%
      names(formals(gf_squareplot))
  ))
  # layer_factory() sets check.param = FALSE, so a name missing from the signature
  # is still forwarded and still works -- confirmed at the grid level above, where
  # `pad` visibly changes the built frame despite never being a named parameter
  expect_false("pad" %in% names(formals(gf_squareplot)))
})
