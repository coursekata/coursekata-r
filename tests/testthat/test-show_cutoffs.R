hist_with <- function(frm) gf_histogram(~Thumb, data = Fingers, binwidth = 5, fill = frm)

test_that("a two-sided fill draws a marker on each side", {
  p <- suppressMessages(show_cutoffs(hist_with(~ middle(Thumb, .95))))
  expect_false(is.na(layer_index(p, "cutoff_lower")))
  expect_false(is.na(layer_index(p, "cutoff_upper")))
})

test_that("a one-sided fill draws one marker", {
  p <- suppressMessages(show_cutoffs(hist_with(~ upper(Thumb, .05))))
  expect_true(is.na(layer_index(p, "cutoff_lower")))
  expect_false(is.na(layer_index(p, "cutoff_upper")))
})

test_that("a non-greedy tail holding no values still draws its marker", {
  d <- data.frame(v = 1:10)
  low <- suppressMessages(show_cutoffs(
    gf_histogram(~v, data = d, binwidth = 1, fill = ~ lower(v, .05, greedy = FALSE))
  ))
  high <- suppressMessages(show_cutoffs(
    gf_histogram(~v, data = d, binwidth = 1, fill = ~ upper(v, .05, greedy = FALSE))
  ))
  expect_false(is.na(layer_index(low, "cutoff_lower_marker")))
  expect_false(is.na(layer_index(high, "cutoff_upper_marker")))
})

test_that("labels are added only when asked for", {
  bare <- suppressMessages(show_cutoffs(hist_with(~ middle(Thumb, .95))))
  expect_true(is.na(layer_index(bare, "cutoff_lower_label")))
})

test_that("each labelled side names its proportion and its direction", {
  h <- hist_with(~ middle(Thumb, .95))
  # from the input plot: the returned plot's own range moves with the labels it placed
  xr <- plot_geometry(h)$x_range
  p <- suppressMessages(show_cutoffs(h, labels = TRUE))
  lower <- p$layers[[layer_index(p, "cutoff_lower_label")]]
  upper <- p$layers[[layer_index(p, "cutoff_upper_label")]]
  expect_equal(lower$aes_params$label, ".025 of\nvalues below")
  expect_equal(upper$aes_params$label, ".025 of\nvalues above")
  expect_gt(lower$data$x, xr[[1]])
  expect_lt(lower$data$x, upper$data$x)
  expect_lt(upper$data$x, xr[[2]])
  expect_false(is.na(layer_index(p, "cutoff_lower_leader")))
  expect_false(is.na(layer_index(p, "cutoff_upper_leader")))
})

drawn_x <- function(p, tag) {
  ggplot2::ggplot_build(p)$data[[layer_index(p, tag)]]$x[[1]]
}

test_that("an untransformed plot puts each label where an identity transform must leave it", {
  p <- suppressMessages(show_cutoffs(hist_with(~ middle(Thumb, .95)), labels = TRUE))
  panel <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range
  expect_equal(drawn_x(p, "cutoff_lower_label"), 43.08)
  expect_equal(drawn_x(p, "cutoff_upper_label"), 85.92)
  expect_lt(drawn_x(p, "cutoff_lower_label"), drawn_x(p, "cutoff_lower_marker"))
  expect_gt(drawn_x(p, "cutoff_upper_label"), drawn_x(p, "cutoff_upper_marker"))
  expect_gt(drawn_x(p, "cutoff_lower_label"), panel[[1]])
  expect_lt(drawn_x(p, "cutoff_upper_label"), panel[[2]])
})

test_that("the labels answer to the data, not to however wide the panel around it is", {
  h <- hist_with(~ middle(Thumb, .95))
  narrow <- suppressMessages(show_cutoffs(h, labels = TRUE))
  wide <- suppressMessages(
    show_cutoffs(h + ggplot2::coord_cartesian(xlim = c(0, 200)), labels = TRUE)
  )
  expect_equal(drawn_x(wide, "cutoff_lower_label"), drawn_x(narrow, "cutoff_lower_label"))
  expect_equal(drawn_x(wide, "cutoff_upper_label"), drawn_x(narrow, "cutoff_upper_label"))
})

test_that("a transformed x scale insets each label by the same visual step at both ends", {
  set.seed(1)
  d <- data.frame(v = 10^runif(200, 0, 3))
  h <- gf_histogram(~v, data = d, bins = 20, fill = ~ middle(v, .95)) + ggplot2::scale_x_log10()
  p <- suppressMessages(show_cutoffs(h, labels = TRUE))
  panel <- diff(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range)
  gap <- function(side) {
    abs(drawn_x(p, paste0("cutoff_", side, "_label")) -
      drawn_x(p, paste0("cutoff_", side, "_marker"))) / panel
  }
  # untransformed the same drawing lands 2.3% and 9.8% of the panel from its marker, so three
  # decades of x staying under 12% says the offset is a visual step and not a raw-unit one
  expect_lt(gap("lower"), 0.12)
  expect_lt(gap("upper"), 0.12)
  expect_equal(
    drawn_x(p, "cutoff_lower_label") - log10(min(d$v)),
    log10(max(d$v)) - drawn_x(p, "cutoff_upper_label")
  )
})

test_that("a one-sided fill labels only the side it shades", {
  p <- suppressMessages(show_cutoffs(hist_with(~ upper(Thumb, .05)), labels = TRUE))
  expect_true(is.na(layer_index(p, "cutoff_lower_label")))
  expect_false(is.na(layer_index(p, "cutoff_upper_label")))
})

test_that("the marker is drawn at the cutoff the plan chose", {
  p <- hist_with(~ middle(Thumb, .95))
  drawn <- suppressMessages(show_cutoffs(p))
  plan <- cutoff_plan(cutoff_spec(plot_spec(p)$resolve_aes("fill")), Fingers$Thumb)
  # the triangle is tagged "_marker"; "cutoff_lower" is the dashed segment above it
  marker <- drawn$layers[[layer_index(drawn, "cutoff_lower_marker")]]
  expect_equal(marker$data$x[[1]], plan$lower)
})

test_that("all five distribution functions are accepted", {
  for (frm in list(
    ~ middle(Thumb, .95), ~ tails(Thumb, .95), ~ outer(Thumb, .05),
    ~ upper(Thumb, .05), ~ lower(Thumb, .05)
  )) {
    expect_s3_class(suppressMessages(show_cutoffs(hist_with(frm))), "ggplot")
  }
})

test_that("a plot with no distribution fill is refused by name", {
  p <- gf_histogram(~Thumb, data = Fingers)
  expect_error(suppressMessages(show_cutoffs(p)), "distribution function")
})

test_that("an x aesthetic that is an expression is refused by name", {
  p <- gf_histogram(~ log(Thumb), data = Fingers, fill = ~ middle(Thumb, .95))
  expect_error(suppressMessages(show_cutoffs(p)), "log\\(Thumb\\)")
})

test_that("an x variable that is not in the plot's data is refused by name", {
  v <- Fingers$Thumb
  p <- gf_histogram(~v, bins = 10, fill = ~ middle(v, .95))
  expect_error(suppressMessages(show_cutoffs(p)), "Can't find `v`", fixed = TRUE)
})

test_that("a plot without cartesian axes says so instead of failing inside arithmetic", {
  p <- gf_histogram(~Thumb, data = Fingers, binwidth = 5, fill = ~ middle(Thumb, .95)) +
    ggplot2::coord_polar()
  expect_error(suppressMessages(show_cutoffs(p)), "cartesian")
})

# the markers are placed outside the position scale, so read them back as a fraction of the
# panel, which is the one thing both a scaled and an unscaled position can be asked for
npc_y <- function(p, tag, which = "y") {
  built <- ggplot2::ggplot_build(p)
  v <- built$data[[layer_index(p, tag)]][[which]]
  if (inherits(v, "AsIs")) return(as.numeric(v))
  yr <- built$layout$panel_params[[1]]$y.range
  (as.numeric(v) - yr[[1]]) / diff(yr)
}

marker_tags <- c(
  "cutoff_lower", "cutoff_upper", "cutoff_lower_marker", "cutoff_upper_marker",
  "cutoff_lower_label", "cutoff_upper_label", "cutoff_lower_leader", "cutoff_upper_leader"
)

test_that("the markers hang below the panel, which only unclipping lets them show", {
  p <- suppressMessages(show_cutoffs(hist_with(~ middle(Thumb, .95))))
  expect_equal(p$coordinates$clip, "off")
  expect_lt(npc_y(p, "cutoff_lower_marker"), 0)
  expect_lt(npc_y(p, "cutoff_upper_marker"), 0)
})

test_that("a transformed count axis still gets its markers", {
  h <- hist_with(~ middle(Thumb, .95)) + ggplot2::scale_y_sqrt()
  expect_no_warning(p <- suppressMessages(show_cutoffs(h, labels = TRUE)))
  expect_no_warning(built <- ggplot2::ggplot_build(p))
  for (tag in marker_tags) {
    d <- built$data[[layer_index(p, tag)]]
    expect_true(all(is.finite(as.numeric(d$y))), label = paste(tag, "y"))
    if (!is.null(d$yend)) {
      expect_true(all(is.finite(as.numeric(d$yend))), label = paste(tag, "yend"))
    }
  }
})

test_that("the markers land in the same place whichever way the count axis is drawn", {
  h <- hist_with(~ middle(Thumb, .95))
  plain <- suppressMessages(show_cutoffs(h, labels = TRUE))
  rooted <- suppressMessages(show_cutoffs(h + ggplot2::scale_y_sqrt(), labels = TRUE))
  for (tag in marker_tags) {
    expect_equal(npc_y(rooted, tag), npc_y(plain, tag), label = tag)
  }
})

test_that("an untransformed plot draws every marker where it always drew it", {
  # the parity gate, stated as an assertion: an identity axis must be left alone, and
  # these five numbers are what the committed snapshot was drawn from
  h <- hist_with(~ middle(Thumb, .95))
  yr <- plot_geometry(h)$y_range
  top <- yr[[2]]
  p <- suppressMessages(show_cutoffs(h, labels = TRUE))
  at <- function(tag, which = "y") yr[[1]] + npc_y(p, tag, which) * diff(yr)
  expect_equal(at("cutoff_lower_marker"), -top * 0.06)
  expect_equal(at("cutoff_lower"), -top * 0.045)
  expect_equal(at("cutoff_lower", "yend"), top * 0.20)
  expect_equal(at("cutoff_lower_label"), top * 0.65)
  expect_equal(at("cutoff_lower_leader", "yend"), top * 0.57)
})

test_that("a flipped plot measures its fractions along the axis the counts are drawn on", {
  h <- hist_with(~ middle(Thumb, .95))
  plain <- suppressMessages(show_cutoffs(h, labels = TRUE))
  flipped <- suppressMessages(show_cutoffs(h + ggplot2::coord_flip(), labels = TRUE))
  for (tag in marker_tags) {
    expect_equal(npc_y(flipped, tag), npc_y(plain, tag), label = tag)
  }
})

test_that("adding the markers leaves the histogram its own count axis", {
  h <- hist_with(~ middle(Thumb, .95))
  expect_equal(
    plot_geometry(suppressMessages(show_cutoffs(h, labels = TRUE)))$y_range,
    plot_geometry(h)$y_range
  )
  f <- h + ggplot2::coord_flip()
  expect_equal(
    plot_geometry(suppressMessages(show_cutoffs(f, labels = TRUE)))$x_range,
    plot_geometry(f)$x_range
  )
})

test_that("drawing the cutoffs twice puts the second set on top of the first", {
  h <- hist_with(~ middle(Thumb, .95))
  twice <- suppressMessages(show_cutoffs(suppressMessages(show_cutoffs(h))))
  ys <- unlist(lapply(twice$layers, function(l) {
    if (identical(attr(l, "coursekata_layer"), "cutoff_lower_marker")) as.numeric(l$data$y)
  }))
  expect_length(ys, 2)
  expect_equal(ys[[1]], ys[[2]])
})

test_that("unclipping keeps the coordinate system the caller chose", {
  h <- hist_with(~ middle(Thumb, .95)) + ggplot2::coord_flip()
  p <- suppressMessages(show_cutoffs(h))
  expect_s3_class(p$coordinates, "CoordFlip")
  expect_equal(p$coordinates$clip, "off")
})

test_that("unclipping keeps the range the caller zoomed to", {
  h <- hist_with(~ middle(Thumb, .95)) + ggplot2::coord_cartesian(xlim = c(50, 70))
  before <- plot_geometry(h)$x_range
  p <- suppressMessages(show_cutoffs(h))
  expect_equal(before, c(49, 71))
  expect_equal(plot_geometry(p)$x_range, before)
})

test_that("show_cutoffs snapshot", {
  skip_if_not_installed("vdiffr")
  p <- gf_histogram(~Thumb, data = Fingers, fill = ~ middle(Thumb, .95), bins = 30)
  suppressMessages(show_cutoffs(p)) %>%
    expect_doppelganger("show_cutoffs-middle-95")
})
