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

# --- an explicit `part` argument -------------------------------------------------

test_that("an explicit part computes the same marker the fill it overrides would have", {
  fill_based <- suppressMessages(show_cutoffs(hist_with(~ middle(Thumb, .95))))
  part_based <- suppressMessages(show_cutoffs(hist_with(NULL), middle(Thumb, .95)))
  # MUTATION: the explicit path computing from different values than the fill path
  expect_equal(
    part_based$layers[[layer_index(part_based, "cutoff_lower_marker")]]$data$x,
    fill_based$layers[[layer_index(fill_based, "cutoff_lower_marker")]]$data$x
  )
})

test_that("an explicit part overrides a mismatched fill instead of deferring to it", {
  h <- hist_with(~ middle(Thumb, .95))
  fill_based <- suppressMessages(show_cutoffs(h))
  overridden <- suppressMessages(show_cutoffs(h, middle(Thumb, .99)))
  # MUTATION: the override being ignored -- the markers would land on top of the fill's
  expect_lt(
    overridden$layers[[layer_index(overridden, "cutoff_lower_marker")]]$data$x,
    fill_based$layers[[layer_index(fill_based, "cutoff_lower_marker")]]$data$x
  )
  expect_gt(
    overridden$layers[[layer_index(overridden, "cutoff_upper_marker")]]$data$x,
    fill_based$layers[[layer_index(fill_based, "cutoff_upper_marker")]]$data$x
  )
})

test_that("show_cutoffs(hist, \"red\") and a symbol holding the same string both refuse by name", {
  h <- hist_with(~ middle(Thumb, .95))
  # MUTATION: checking only for a string literal, which misses the second and third calls
  expect_error(show_cutoffs(h, "red"), "distribution part")
  col <- "red"
  expect_error(show_cutoffs(h, col), "distribution part")
  expect_error(show_cutoffs(h, Thumb), "distribution part")
})

test_that("an explicit part naming a variable the plot's data has not got names both", {
  hist_of_b1 <- gf_histogram(~b1, data = data.frame(b1 = 1:20), bins = 5)
  # MUTATION: a raw `object 'nonexistent' not found` from eval_tidy, naming neither variable
  err <- rlang::catch_cnd(show_cutoffs(hist_of_b1, middle(nonexistent, .95)))
  expect_match(conditionMessage(err), "b1")
  expect_match(conditionMessage(err), "nonexistent")
})

test_that("a part naming a variable the plot does not put on x is refused by its axis", {
  p <- gf_point(Thumb ~ Height, data = Fingers)
  # MUTATION: the x-agreement check being skipped
  expect_error(show_cutoffs(p, middle(Thumb, .95)), "x axis")
})

test_that("a part matching x on a non-distribution plot is still refused, by its shape", {
  p <- gf_point(Thumb ~ Height, data = Fingers)
  # MUTATION: the distribution-shape guard omitted, letting quantile markers hang under a
  # scatterplot whenever the part happens to name the variable already on x
  expect_error(show_cutoffs(p, middle(Height, .95)), "marks cutoffs on a distribution")
})

test_that("an explicit part still works on the package's own distribution geom", {
  p <- gf_squareplot(~Thumb, data = Fingers)
  # MUTATION: the distribution geom list omitting GeomSquareplot
  expect_s3_class(suppressMessages(show_cutoffs(p, middle(Thumb, .95))), "ggplot")
})

test_that("a plot that draws no layers at all is refused, not crashed on", {
  # MUTATION: indexing plot$layers[[1]] with no length guard, which throws a
  # raw "subscript out of bounds" instead of the package's own refusal
  p <- ggplot2::ggplot(Fingers, ggplot2::aes(x = Thumb, fill = middle(Thumb, .95)))
  expect_error(show_cutoffs(p), "marks cutoffs on a distribution")
})

# --- stacking ---------------------------------------------------------------------

test_that("stacking two calls keeps both complete sets of markers", {
  base <- hist_with(~ middle(Thumb, .95))
  twice <- suppressMessages(show_cutoffs(suppressMessages(show_cutoffs(base)), middle(Thumb, .99)))
  idx <- layer_indices(twice, "cutoff_lower_marker")
  # MUTATION: stacking dropping a set, or the second call re-reading the first's plan
  expect_length(idx, 2)
  xs <- vapply(idx, function(i) twice$layers[[i]]$data$x, numeric(1))
  expect_equal(sort(xs), sort(c(
    cutoff_plan(list(func = "middle", prop = .95, greedy = TRUE), Fingers$Thumb)$lower,
    cutoff_plan(list(func = "middle", prop = .99, greedy = TRUE), Fingers$Thumb)$lower
  )))
})

test_that("a second stacked call's label and leader land where a solo call would put them", {
  base <- hist_with(~ middle(Thumb, .95))
  solo <- suppressMessages(show_cutoffs(base, middle(Thumb, .99), labels = TRUE))
  stacked <- suppressWarnings(suppressMessages(show_cutoffs(
    suppressMessages(show_cutoffs(base, labels = TRUE)),
    middle(Thumb, .99), labels = TRUE
  )))

  solo_label <- solo$layers[[layer_index(solo, "cutoff_lower_label")]]
  stacked_label <- stacked$layers[[layer_indices(stacked, "cutoff_lower_label")[[2]]]]
  # MUTATION: the rebuilt geometry moving label_x or label_y between a solo and a stacked call
  expect_equal(stacked_label$data$x, solo_label$data$x)
  expect_equal(stacked_label$data$y, solo_label$data$y)

  solo_leader <- solo$layers[[layer_index(solo, "cutoff_lower_leader")]]
  stacked_leader <- stacked$layers[[layer_indices(stacked, "cutoff_lower_leader")[[2]]]]
  expect_equal(stacked_leader$data$x, solo_leader$data$x)
  expect_equal(stacked_leader$data$xend, solo_leader$data$xend)
  expect_equal(stacked_leader$data$y, solo_leader$data$y)
  expect_equal(stacked_leader$data$yend, solo_leader$data$yend)

  # the marker's npc y is a fixed fraction of the panel and is unaffected by the rebuild
  expect_equal(npc_y(stacked, "cutoff_lower_marker"), npc_y(solo, "cutoff_lower_marker"))
})

test_that("three stacked calls wrap the coord exactly once and leave clip off", {
  base <- hist_with(~ middle(Thumb, .95))
  thrice <- suppressWarnings(suppressMessages(show_cutoffs(
    suppressMessages(show_cutoffs(suppressMessages(show_cutoffs(base)), middle(Thumb, .99))),
    middle(Thumb, .90)
  )))
  expect_equal(thrice$coordinates$clip, "off")
  parent <- get("super", envir = thrice$coordinates)()
  # MUTATION: the ggproto chain deepening on every stacked call instead of staying one deep
  expect_false(isTRUE(attr(parent, "coursekata_cutoff_unclipped")))
})

test_that("two labeled calls warn that their labels overlap, and still return a plot", {
  base <- hist_with(~ middle(Thumb, .95))
  once <- suppressMessages(show_cutoffs(base, labels = TRUE))
  # MUTATION: the warning missing, or escalated to an abort that drops the second set
  expect_warning(
    twice <- suppressMessages(show_cutoffs(once, middle(Thumb, .99), labels = TRUE)),
    class = "coursekata_cutoff_labels_overlap"
  )
  expect_s3_class(twice, "ggplot")
})

test_that("one labeled call does not warn", {
  base <- hist_with(~ middle(Thumb, .95))
  # MUTATION: an over-eager warning firing on the documented single-call default
  expect_no_warning(suppressMessages(show_cutoffs(base, labels = TRUE)))
})

test_that("show_cutoffs stacking snapshot", {
  skip_if_not_installed("vdiffr")
  p <- gf_histogram(~Thumb, data = Fingers, bins = 30)
  suppressWarnings(suppressMessages(
    p %>%
      show_cutoffs(middle(Thumb, .999)) %>%
      show_cutoffs(middle(Thumb, .95)) %>%
      show_cutoffs(middle(Thumb, .80), labels = TRUE)
  )) %>%
    expect_doppelganger("show_cutoffs-stacked-three-levels")
})

# --- StatCutoff ---------------------------------------------------------------------

test_that("StatCutoff's built xintercepts equal cutoff_plan()'s for all five distribution parts", {
  values <- Fingers$Thumb
  for (fn in c("middle", "tails", "outer", "upper", "lower")) {
    prop <- if (fn %in% c("upper", "lower")) .05 else .95
    plan <- cutoff_plan(list(func = fn, prop = prop, greedy = TRUE), values)
    expected <- c(plan$lower, plan$upper)
    expected <- expected[!is.na(expected)]

    built <- StatCutoff$compute_panel(data.frame(x = values), scales = NULL, func = fn, prop = prop)
    # MUTATION: the stat and cutoff_plan() drifting apart for any one of the five parts
    expect_equal(sort(built$xintercept), sort(expected), label = fn)
  }
})

test_that("StatCutoff computes per panel while show_cutoffs repeats one plan across panels", {
  built_stat <- ggplot2::ggplot_build(
    ggplot2::ggplot(Fingers, ggplot2::aes(x = Thumb)) +
      ggplot2::layer(
        stat = StatCutoff, geom = ggplot2::GeomVline, position = "identity",
        params = list(func = "middle", prop = .5, na.rm = TRUE)
      ) +
      ggplot2::facet_wrap(~Sex)
  )
  dat <- built_stat$data[[1]]
  panel_cutoffs <- unique(lapply(split(dat$xintercept, dat$PANEL), sort))
  # MUTATION: the stat aggregating across panels instead of per panel, becoming one repeated plan
  expect_gt(length(panel_cutoffs), 1)

  h <- gf_histogram(~Thumb | Sex, data = Fingers, binwidth = 5, fill = ~ middle(Thumb, .5))
  p <- suppressMessages(show_cutoffs(h))
  built_show <- ggplot2::ggplot_build(p)
  marker_x <- built_show$data[[layer_index(p, "cutoff_lower_marker")]]$x
  # MUTATION: show_cutoffs() accidentally computing per panel instead of from the whole data
  expect_length(unique(marker_x), 1)
})

test_that("StatCutoff refuses a part it does not have and a proportion that is not one", {
  # MUTATION: no `setup_params`. `show_cutoffs()` reaches `cutoff_plan()`
  # through `cutoff_spec()`, which reads a call and can refuse a name it does
  # not know; this route is handed plain values by someone writing a layer by
  # hand and nothing was reading them. Measured before the check:
  # `func = "bogus"` fell through the switch and marked a lower cutoff, and
  # `prop = 2` returned an "upper" cutoff sitting on the smallest observation.
  # Both drew a mark indistinguishable from a real one.
  marked <- function(...) {
    ggplot2::ggplot_build(
      gf_histogram(~Thumb, data = Fingers, binwidth = 5) +
        ggplot2::layer(
          stat = StatCutoff, geom = ggplot2::GeomVline, position = "identity",
          params = list(na.rm = TRUE, ...)
        )
    )
  }

  expect_error(marked(func = "bogus"), "names the part of the distribution")
  expect_error(marked(func = 42), "names the part of the distribution")
  expect_error(marked(prop = 2), "proportion")
  expect_error(marked(prop = -1), "proportion")
  expect_error(marked(prop = c(.1, .2)), "proportion")
  expect_no_error(marked(func = "upper", prop = .05))
})

test_that("a cutoff marks the reader's own upper tail on a scale that runs backwards", {
  # MUTATION: planning from `data$x` as it arrives. A cutoff is a quantile;
  # quantiles survive a transformation that increases and turn over under one
  # that decreases. Measured before the fix, an upper cutoff on 1:10 under
  # `scale_x_reverse()` landed on 2 -- the reader's LOWER tail, drawn with
  # every appearance of being the upper one.
  d <- data.frame(x = 1:10)
  plain <- ggplot2::ggplot(d, ggplot2::aes(x = x)) + ggplot2::geom_histogram(bins = 10)
  cut <- function(...) {
    ggplot2::layer(
      stat = StatCutoff, geom = ggplot2::GeomVline, position = "identity",
      params = list(na.rm = TRUE, ...)
    )
  }
  drawn <- function(p) ggplot2::ggplot_build(p)$data[[2]]$xintercept

  # forward: the upper 5% of 1:10 is the top of the range
  expect_equal(drawn(plain + cut(func = "upper", prop = .05)), 10)
  # reversed: the same request marks the same VALUE, which the reversed scale
  # stores negated -- not the value at the other end of the data
  expect_equal(drawn(plain + cut(func = "upper", prop = .05) + ggplot2::scale_x_reverse()), -10)
  expect_equal(drawn(plain + cut(func = "lower", prop = .05) + ggplot2::scale_x_reverse()), -1)
})
