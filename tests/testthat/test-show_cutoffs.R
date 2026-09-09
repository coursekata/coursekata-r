cutoff_test_histogram <- function(values = 1:10) {
  ggplot2::ggplot(data.frame(x = values), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = 0.5)
}

cutoff_test_layers <- function(plot) {
  Filter(function(layer) inherits(layer$geom, "GeomCutoff"), plot$layers)
}

cutoff_test_expected <- function(part, prop, values, greedy = TRUE) {
  plan <- cutoff_plan(
    list(func = part, prop = prop, greedy = greedy),
    values
  )
  anchors <- unlist(plan[c("lower", "upper")], use.names = FALSE)
  anchors[!is.na(anchors)]
}

cutoff_test_draw <- function(plot, width = 8, height = 6) {
  grDevices::pdf(NULL, width = width, height = height)
  on.exit(grDevices::dev.off())
  grid::grid.newpage()
  grid::grid.draw(ggplot2::ggplotGrob(plot))
  TRUE
}

cutoff_test_layout <- function(plot, width = 160, height = 110,
                               panel_index = 1L) {
  built <- suppressWarnings(ggplot2::ggplot_build(plot))
  layer_index <- which(vapply(built$plot$layers, function(layer) {
    inherits(layer$geom, "GeomCutoffCallout")
  }, logical(1)))
  stopifnot(length(layer_index) == 1L)
  layer <- built$plot$layers[[layer_index]]
  data <- built$data[[layer_index]]
  if ("PANEL" %in% names(data)) {
    data <- data[as.integer(data$PANEL) == panel_index, , drop = FALSE]
  }
  panel_params <- built$layout$panel_params[[panel_index]]
  panel <- cutoff_panel_data(data, panel_params, built$plot$coordinates)
  stopifnot(!is.null(panel))
  avoidance <- cutoff_avoidance_panel(
    layer$geom_params$avoidance, panel_params, built$plot$coordinates
  )

  grid::pushViewport(grid::viewport(
    width = grid::unit(width, "mm"), height = grid::unit(height, "mm")
  ))
  on.exit(grid::popViewport())
  measured <- measure_cutoff_callouts(panel$boundary, 3.2, 1.6)
  layout <- solve_cutoff_callout_layout(
    measured$data, horizontal = panel$horizontal, height = 0.2,
    metrics = measured$metrics, panel_width = measured$panel_width,
    panel_height = measured$panel_height, avoidance = avoidance
  )
  layout$data <- measured$data
  layout$panel_width <- width
  layout$panel_height <- height
  layout
}

cutoff_test_boxes_overlap <- function(first, second, gap = 1.5) {
  first$x1 < second$x2 + gap && first$x2 > second$x1 - gap &&
    first$y1 < second$y2 + gap && first$y2 > second$y1 - gap
}

expect_complete_cutoff_layout <- function(layout, count,
                                          allow_overlap = FALSE) {
  expect_length(layout$boxes, count)
  expect_length(layout$routes, count)
  expect_true(all(vapply(layout$boxes, function(box) {
    box$x1 >= 0 && box$x2 <= layout$panel_width &&
      box$y1 >= 0 && box$y2 <= layout$panel_height
  }, logical(1))))

  if (count > 1L && !allow_overlap) {
    pairs <- utils::combn(seq_len(count), 2L)
    expect_false(any(apply(pairs, 2L, function(pair) {
      cutoff_test_boxes_overlap(
        layout$boxes[[pair[[1L]]]], layout$boxes[[pair[[2L]]]]
      )
    })))
  }

  for (i in seq_len(count)) {
    row <- layout$data[i, , drop = FALSE]
    source <- if (layout$horizontal) {
      inward <- sign(row$.opposite_x - row$x)
      c(
        row$x * layout$panel_width + inward * min(
          0.2 * layout$panel_width,
          abs(row$.opposite_x - row$x) * layout$panel_width
        ),
        row$y * layout$panel_height
      )
    } else {
      inward <- sign(row$.opposite_y - row$y)
      c(
        row$x * layout$panel_width,
        row$y * layout$panel_height + inward * min(
          0.2 * layout$panel_height,
          abs(row$.opposite_y - row$y) * layout$panel_height
        )
      )
    }
    route <- layout$routes[[i]]
    expect_equal(unname(route[1L, ]), source)

    box <- layout$boxes[[i]]
    ports <- rbind(
      right = c(box$x2, box$y), top = c(box$x, box$y2),
      left = c(box$x1, box$y), bottom = c(box$x, box$y1)
    )
    nearest <- which.min(rowSums((ports - rep(source, each = 4L))^2))
    expect_equal(unname(route[nrow(route), ]), unname(ports[nearest, ]))
  }
}

test_that("fill inference makes one truthful self-contained layer", {
  base <- gf_histogram(
    ~Thumb, data = Fingers, binwidth = 5,
    fill = ~middle(Thumb, .95)
  )
  expected <- cutoff_test_expected("middle", .95, Fingers$Thumb)
  out <- suppressMessages(show_cutoffs(base, show_labels = TRUE))

  layers <- cutoff_test_layers(out)
  expect_length(layers, 1)
  expect_null(out$scales$get_scales("x"))

  layer <- layers[[1]]
  expect_identical(attr(layer, "coursekata_layer"), "distribution_cutoff")
  expect_identical(
    attr(layer, "coursekata_cutoff_style"),
    list(
      colour = "#1e3a8a", fill = "white", linetype = "dashed",
      linewidth = 0.5
    )
  )
  expect_identical(layer$inherit.aes, FALSE)
  expect_identical(layer$show.legend, FALSE)
  expect_identical(layer$aes_params$colour, "#1e3a8a")
  expect_identical(layer$aes_params$linewidth, 0.5)
  expect_equal(layer$data$xintercept, expected)
  expect_equal(layer$data$.value, expected)
  expect_identical(layer$data$.coursekata_protect, c(TRUE, TRUE))
  expect_identical(layer$data$side, c("lower", "upper"))
  expect_identical(layer$data$call_id, c(1L, 1L))
  expect_equal(
    layer$data$label,
    c(".025 of\nvalues below", ".025 of\nvalues above")
  )
  expect_identical(layer$geom_params$marker, TRUE)
  expect_identical(layer$geom_params$marker_size, 4)
})

test_that("labels are layer metadata and appear only when requested", {
  base <- cutoff_test_histogram()
  bare <- suppressMessages(show_cutoffs(base, middle(x, .8)))
  labelled <- suppressMessages(show_cutoffs(
    base, middle(x, .8), color = "purple", size = 7,
    show_labels = TRUE
  ))

  bare_layer <- cutoff_test_layers(bare)[[1]]
  labelled_layer <- cutoff_test_layers(labelled)[[1]]
  expect_true(all(is.na(bare_layer$data$label)))
  expect_equal(
    labelled_layer$data$label,
    c(".1 of\nvalues below", ".1 of\nvalues above")
  )
  expect_identical(labelled_layer$data$side, c("lower", "upper"))
  expect_identical(labelled_layer$aes_params$colour, "purple")
  expect_identical(labelled_layer$geom_params$marker_size, 7)
})

test_that("all five distribution parts preserve cutoff_plan semantics", {
  values <- c(1:20, NA_real_)
  base <- cutoff_test_histogram(values)
  cases <- list(
    middle = list(
      plot = suppressMessages(show_cutoffs(base, middle(x, .8))),
      prop = .8
    ),
    tails = list(
      plot = suppressMessages(show_cutoffs(base, tails(x, .8))),
      prop = .8
    ),
    outer = list(
      plot = suppressMessages(show_cutoffs(base, outer(x, .2))),
      prop = .2
    ),
    upper = list(
      plot = suppressMessages(show_cutoffs(base, upper(x, .2))),
      prop = .2
    ),
    lower = list(
      plot = suppressMessages(show_cutoffs(base, lower(x, .2))),
      prop = .2
    )
  )

  for (part in names(cases)) {
    layer <- cutoff_test_layers(cases[[part]]$plot)[[1]]
    expect_equal(
      layer$data$xintercept,
      cutoff_test_expected(part, cases[[part]]$prop, values),
      label = part
    )
  }
  expect_identical(cutoff_test_layers(cases$upper$plot)[[1]]$data$side, "upper")
  expect_identical(cutoff_test_layers(cases$lower$plot)[[1]]$data$side, "lower")
})

test_that("greediness, ties, missing values, and empty one-sided tails are preserved", {
  tied <- c(1, 1, 2, 2, 3, 3, 4, 4, NA_real_)
  tied_base <- cutoff_test_histogram(tied)
  greedy <- suppressMessages(show_cutoffs(
    tied_base, middle(x, .5, greedy = TRUE)
  ))
  conservative <- suppressMessages(show_cutoffs(
    tied_base, middle(x, .5, greedy = FALSE)
  ))
  expect_equal(
    cutoff_test_layers(greedy)[[1]]$data$xintercept,
    cutoff_test_expected("middle", .5, tied, greedy = TRUE)
  )
  expect_equal(
    cutoff_test_layers(conservative)[[1]]$data$xintercept,
    cutoff_test_expected("middle", .5, tied, greedy = FALSE)
  )

  small <- cutoff_test_histogram(1:10)
  low <- suppressMessages(show_cutoffs(
    small, lower(x, .05, greedy = FALSE)
  ))
  high <- suppressMessages(show_cutoffs(
    small, upper(x, .05, greedy = FALSE)
  ))
  expect_identical(as.numeric(cutoff_test_layers(low)[[1]]$data$xintercept), 1)
  expect_identical(as.numeric(cutoff_test_layers(high)[[1]]$data$xintercept), 10)
})

test_that("an explicit part can supply or override the fill-derived plan", {
  no_fill <- gf_histogram(~Thumb, data = Fingers, binwidth = 5)
  explicit <- suppressMessages(show_cutoffs(no_fill, middle(Thumb, .95)))
  expect_equal(
    cutoff_test_layers(explicit)[[1]]$data$xintercept,
    cutoff_test_expected("middle", .95, Fingers$Thumb)
  )

  shaded <- gf_histogram(
    ~Thumb, data = Fingers, binwidth = 5,
    fill = ~middle(Thumb, .95)
  )
  inferred <- suppressMessages(show_cutoffs(shaded))
  overridden <- suppressMessages(show_cutoffs(shaded, middle(Thumb, .99)))
  inferred_at <- cutoff_test_layers(inferred)[[1]]$data$xintercept
  overridden_at <- cutoff_test_layers(overridden)[[1]]$data$xintercept

  expect_lt(overridden_at[[1]], inferred_at[[1]])
  expect_gt(overridden_at[[2]], inferred_at[[2]])
  expect_equal(
    overridden_at,
    cutoff_test_expected("middle", .99, Fingers$Thumb)
  )
})

test_that("show_cutoffs plans exactly once", {
  base <- gf_histogram(
    ~Thumb, data = Fingers, binwidth = 5,
    fill = ~middle(Thumb, .95)
  )
  original_spec <- cutoff_spec
  original_plan <- cutoff_plan
  spec_calls <- 0L
  plan_calls <- 0L
  local_mocked_bindings(
    cutoff_spec = function(...) {
      spec_calls <<- spec_calls + 1L
      original_spec(...)
    },
    cutoff_plan = function(...) {
      plan_calls <<- plan_calls + 1L
      original_plan(...)
    },
    .package = "coursekata"
  )

  suppressMessages(show_cutoffs(base))
  expect_identical(spec_calls, 1L)
  expect_identical(plan_calls, 1L)
})

test_that("public plot and labels aliases keep the legacy call shape", {
  withr::local_options(lifecycle_verbosity = "default")
  cache <- get("deprecation_env", asNamespace("lifecycle"))
  ids <- c("coursekata-show_cutoffs-plot", "coursekata-show_cutoffs-labels")
  clear <- function() {
    present <- ids[vapply(ids, exists, logical(1), envir = cache, inherits = FALSE)]
    if (length(present) > 0L) rlang::env_unbind(cache, present)
  }
  clear()
  withr::defer(clear())

  base <- gf_histogram(~Thumb, data = Fingers, binwidth = 5)
  legacy_plot <- NULL
  expect_warning(
    legacy_plot <- suppressMessages(
      show_cutoffs(plot = base, middle(Thumb, .95))
    ),
    class = "lifecycle_warning_deprecated"
  )
  expect_s3_class(legacy_plot, "ggplot")
  expect_equal(
    cutoff_test_layers(legacy_plot)[[1]]$data$xintercept,
    cutoff_test_expected("middle", .95, Fingers$Thumb)
  )

  clear()
  legacy_labels <- NULL
  expect_warning(
    legacy_labels <- suppressMessages(
      show_cutoffs(base, middle(Thumb, .95), labels = TRUE)
    ),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(
    cutoff_test_layers(legacy_labels)[[1]]$data$label,
    c(".025 of\nvalues below", ".025 of\nvalues above")
  )

  expect_error(
    show_cutoffs(object = base, plot = base, part = middle(Thumb, .95)),
    "both `object` and deprecated `plot`"
  )
  expect_error(
    show_cutoffs(
      base, middle(Thumb, .95), show_labels = FALSE, labels = TRUE
    ),
    "both `show_labels` and deprecated `labels`"
  )
})

test_that("invalid plots and parts are refused in the caller's vocabulary", {
  distribution <- gf_histogram(~Thumb, data = Fingers, binwidth = 5)
  expect_error(show_cutoffs(distribution), "distribution function")
  expect_error(show_cutoffs(distribution, "red"), "distribution part")
  colour_name <- "red"
  expect_error(show_cutoffs(distribution, colour_name), "distribution part")
  expect_error(show_cutoffs(distribution, Thumb), "distribution part")

  expression_plot <- gf_histogram(
    ~log(Thumb), data = Fingers, binwidth = .05,
    fill = ~middle(log(Thumb), .95)
  )
  expect_error(show_cutoffs(expression_plot), "log\\(Thumb\\)")

  external <- Fingers$Thumb
  external_plot <- gf_histogram(
    ~external, bins = 10, fill = ~middle(external, .95)
  )
  expect_error(show_cutoffs(external_plot), "Can't find `external`", fixed = TRUE)

  other_x <- gf_histogram(~Height, data = Fingers, binwidth = 5)
  mismatch <- rlang::catch_cnd(show_cutoffs(other_x, middle(Thumb, .95)))
  expect_match(conditionMessage(mismatch), "Height")
  expect_match(conditionMessage(mismatch), "Thumb")
  expect_match(conditionMessage(mismatch), "x axis")

  scatter <- gf_point(Thumb ~ Height, data = Fingers)
  expect_error(
    show_cutoffs(scatter, middle(Height, .95)),
    "marks cutoffs on a distribution"
  )
  empty <- ggplot2::ggplot(Fingers, ggplot2::aes(
    x = Thumb, fill = middle(Thumb, .95)
  ))
  expect_error(show_cutoffs(empty), "marks cutoffs on a distribution")

  polar <- gf_histogram(
    ~Thumb, data = Fingers, binwidth = 5,
    fill = ~middle(Thumb, .95)
  ) + ggplot2::coord_polar()
  expect_error(show_cutoffs(polar), "cartesian")

  discrete <- gf_bar(~Sex, data = Fingers)
  expect_error(show_cutoffs(discrete, upper(Sex, .05)), "numeric")
  expect_error(show_cutoffs(distribution, middle(Thumb, .95), show_labels = 1),
    "TRUE.*FALSE"
  )
  expect_error(show_cutoffs(distribution, middle(Thumb, .95), size = -1),
    "non-negative"
  )
  expect_error(
    show_cutoffs(cutoff_test_histogram(c(NA_real_, NA_real_)), middle(x, .95)),
    "no non-missing"
  )
})

test_that("the package distribution geom accepts an explicit part", {
  square <- gf_squareplot(~Thumb, data = Fingers)
  expect_s3_class(
    suppressMessages(show_cutoffs(square, middle(Thumb, .95))),
    "ggplot"
  )
})

test_that("labelled one-sided and single-valued distributions draw", {
  base <- cutoff_test_histogram(1:10)
  lower_plot <- suppressMessages(show_cutoffs(
    base, lower(x, .2), show_labels = TRUE
  ))
  upper_plot <- suppressMessages(show_cutoffs(
    base, upper(x, .2), show_labels = TRUE
  ))
  constant <- cutoff_test_histogram(rep(5, 10))
  constant_plot <- suppressMessages(show_cutoffs(
    constant, middle(x, .8), show_labels = TRUE
  ))

  lower_layout <- cutoff_test_layout(lower_plot)
  upper_layout <- cutoff_test_layout(upper_plot)
  constant_layout <- cutoff_test_layout(constant_plot)

  expect_complete_cutoff_layout(lower_layout, 1L)
  expect_complete_cutoff_layout(upper_layout, 1L)
  expect_complete_cutoff_layout(constant_layout, 2L)
  expect_identical(lower_layout$physical, "lower")
  expect_identical(upper_layout$physical, "upper")
})

test_that("avoidance profiles handle degenerate distributions", {
  expect_equal(
    cutoff_avoidance_profile(c(NA_real_, Inf)),
    data.frame(value = numeric(), height = numeric())
  )
  expect_equal(
    cutoff_avoidance_profile(rep(5, 10)),
    data.frame(value = c(4, 5, 6), height = c(0, .86, 0))
  )
})

test_that("all supported distribution geoms render labelled callouts", {
  values <- data.frame(x = rep(1:20, each = 2))
  plots <- list(
    histogram = ggplot2::ggplot(values, ggplot2::aes(x)) +
      ggplot2::geom_histogram(binwidth = 1),
    density = ggplot2::ggplot(values, ggplot2::aes(x)) +
      ggplot2::geom_density(),
    dotplot = ggplot2::ggplot(values, ggplot2::aes(x)) +
      ggplot2::geom_dotplot(binwidth = 1),
    bar = ggplot2::ggplot(values, ggplot2::aes(x)) + ggplot2::geom_bar(),
    squareplot = gf_squareplot(~x, data = values)
  )

  for (name in names(plots)) {
    marked <- suppressMessages(show_cutoffs(
      plots[[name]], middle(x, .8), show_labels = TRUE
    ))
    expect_true(
      suppressWarnings(suppressMessages(cutoff_test_draw(marked))),
      label = paste(name, "upright")
    )
    expect_true(
      suppressWarnings(suppressMessages(cutoff_test_draw(
        marked + ggplot2::coord_flip()
      ))),
      label = paste(name, "flipped")
    )
  }
})

test_that("pinned expression plots mark the values that were drawn", {
  original <- gf_histogram(
    ~log(Thumb), data = Fingers, binwidth = .05,
    fill = ~middle(log(Thumb), .95)
  )
  pinned <- pin_plot_values(original)$plot
  out <- suppressMessages(show_cutoffs(pinned))

  expect_true("x" %in% names(plot_pins(pinned)))
  expect_no_match(names(cutoff_test_layers(out)[[1]]$data), ".coursekata_pin_",
    fixed = TRUE
  )
  expect_equal(
    cutoff_test_layers(out)[[1]]$data$xintercept,
    cutoff_test_expected("middle", .95, log(Fingers$Thumb))
  )
})

test_that("an explicit part matches a pinned expression by its reader-facing name", {
  original <- gf_histogram(~log(Thumb), data = Fingers, binwidth = .05)
  pinned <- pin_plot_values(original)$plot

  expect_no_error(out <- suppressMessages(
    show_cutoffs(pinned, middle(log(Thumb), .9))
  ))
  expect_equal(
    cutoff_test_layers(out)[[1]]$data$xintercept,
    cutoff_test_expected("middle", .9, log(Fingers$Thumb))
  )
})

test_that("facets repeat one whole-distribution plan in every panel", {
  base <- gf_histogram(~Thumb | Sex, data = Fingers, binwidth = 5)
  out <- suppressMessages(show_cutoffs(
    base, middle(Thumb, .5), show_labels = TRUE
  ))
  expected <- cutoff_test_expected("middle", .5, Fingers$Thumb)
  built <- suppressWarnings(ggplot2::layer_data(out, length(out$layers)))
  by_panel <- lapply(split(built$xintercept, built$PANEL), sort)

  expect_length(by_panel, length(unique(Fingers$Sex)))
  for (anchors in by_panel) expect_equal(anchors, sort(expected))
  expect_equal(cutoff_test_layers(out)[[1]]$data$xintercept, expected)
  expect_true(suppressWarnings(cutoff_test_draw(out)))
})

test_that("global cutoff stems do not retrain free x facets", {
  values <- data.frame(
    x = 1:120,
    panel = rep(c("low", "middle", "high"), each = 40)
  )
  base <- ggplot2::ggplot(values, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(binwidth = 5) +
    ggplot2::facet_wrap(~panel, scales = "free_x")
  before <- lapply(
    ggplot2::ggplot_build(base)$layout$panel_params,
    function(panel) panel$x.range
  )
  out <- suppressMessages(show_cutoffs(
    base, middle(x, .8), show_labels = TRUE
  ))
  after <- lapply(
    ggplot2::ggplot_build(out)$layout$panel_params,
    function(panel) panel$x.range
  )

  expect_equal(after, before)
  expect_equal(
    as.numeric(cutoff_test_layers(out)[[1]]$data$xintercept),
    cutoff_test_expected("middle", .8, values$x)
  )
  grobs <- ggplot2::layer_grob(out, length(out$layers))
  expect_true(all(vapply(
    grobs[1:2], inherits, logical(1), "coursekata_cutoff_callouts"
  )))
  expect_s3_class(grobs[[3L]], "zeroGrob")
  expect_true(suppressWarnings(cutoff_test_draw(out)))
})

test_that("repeated calls retain stable stems and share one callout coordinator", {
  base <- cutoff_test_histogram(1:100)
  out <- suppressMessages(
    base |>
      show_cutoffs(middle(x, .8), show_labels = TRUE) |>
      show_cutoffs(middle(x, .9), show_labels = TRUE) |>
      show_cutoffs(middle(x, .98), show_labels = TRUE)
  )
  layers <- cutoff_test_layers(out)

  expect_length(layers, 3)
  expect_identical(
    unname(vapply(layers, function(layer) unique(layer$data$call_id), integer(1))),
    1:3
  )
  expect_equal(
    unname(lapply(layers, function(layer) layer$data$xintercept)),
    list(
      cutoff_test_expected("middle", .8, 1:100),
      cutoff_test_expected("middle", .9, 1:100),
      cutoff_test_expected("middle", .98, 1:100)
    )
  )
  coordinators <- Filter(
    function(layer) inherits(layer$geom, "GeomCutoffCallout"), out$layers
  )
  expect_length(coordinators, 1L)
  expect_identical(
    attr(coordinators[[1L]], "coursekata_layer"),
    "distribution_cutoff_callouts"
  )
  expect_identical(coordinators[[1L]]$data$call_id, rep(1:3, each = 2L))
  expect_no_warning(ggplot2::ggplotGrob(out))
})

test_that("standalone cutoff layers are not collected as helper metadata", {
  data <- data.frame(
    xintercept = I(c(2, 8)), .value = c(2, 8),
    .coursekata_protect = TRUE,
    label = c("low", "high"), side = c("lower", "upper"), call_id = 1L
  )
  mapping <- ggplot2::aes(
    xintercept = .data$xintercept, .value = .data$.value,
    .coursekata_protect = .data$.coursekata_protect,
    label = .data$label, side = .data$side, call_id = .data$call_id
  )
  plot <- cutoff_test_histogram() + geom_cutoff(
    data = data, mapping = mapping, inherit.aes = FALSE
  )

  out <- update_cutoff_callout_layer(plot)

  expect_identical(out, plot)
  expect_identical(
    layer_indices(out, "distribution_cutoff_callouts"), integer()
  )
})

test_that("one to three overlays lay out at default and narrow sizes", {
  base <- gf_histogram(~Thumb, data = Fingers, bins = 30)
  once <- suppressMessages(show_cutoffs(
    base, middle(Thumb, .999), show_labels = TRUE
  ))
  twice <- suppressMessages(show_cutoffs(
    once, middle(Thumb, .95), color = "firebrick", show_labels = TRUE
  ))
  three <- suppressMessages(show_cutoffs(
    twice, middle(Thumb, .80), color = "darkgreen", show_labels = TRUE
  ))

  for (n in seq_along(list(once, twice, three))) {
    plot <- list(once, twice, three)[[n]]
    for (flipped in c(FALSE, TRUE)) {
      if (flipped) plot <- plot + ggplot2::coord_flip()
      default <- cutoff_test_layout(plot)
      narrow <- cutoff_test_layout(plot, width = 80, height = 60)
      narrow_again <- cutoff_test_layout(plot, width = 80, height = 60)

      expect_complete_cutoff_layout(default, 2L * n)
      expect_complete_cutoff_layout(
        narrow, 2L * n, allow_overlap = n == 3L
      )
      expect_equal(narrow$boxes, narrow_again$boxes)
      expect_equal(narrow$routes, narrow_again$routes)

      if (n > 1L) {
        pairs <- utils::combn(seq_len(2L * n), 2L)
        expect_identical(sum(apply(pairs, 2L, function(pair) {
          cutoff_route_conflict(
            default$routes[[pair[[1L]]]], default$routes[[pair[[2L]]]]
          )
        })), 0)
      }
      expect_identical(sum(vapply(seq_along(default$routes), function(i) {
        sum(vapply(default$boxes[-i], function(box) {
          cutoff_route_box_hits(default$routes[[i]], box)
        }, numeric(1)))
      }, numeric(1))), 0)
    }
  }

  tiny <- cutoff_test_layout(
    three + ggplot2::coord_flip(), width = 25, height = 20
  )
  expect_complete_cutoff_layout(tiny, 6L, allow_overlap = TRUE)
})

test_that("adding calls does not mutate plots the caller retained", {
  base <- cutoff_test_histogram(1:100)
  once <- suppressMessages(show_cutoffs(
    base, middle(x, .8), show_labels = TRUE
  ))
  once_layer_count <- length(once$layers)
  once_stem_data <- cutoff_test_layers(once)[[1]]$data
  once_callout_data <- Filter(
    function(layer) inherits(layer$geom, "GeomCutoffCallout"), once$layers
  )[[1]]$data
  twice <- suppressMessages(show_cutoffs(
    once, middle(x, .95), show_labels = TRUE
  ))

  expect_length(once$layers, once_layer_count)
  expect_identical(cutoff_test_layers(once)[[1]]$data, once_stem_data)
  expect_identical(
    Filter(
      function(layer) inherits(layer$geom, "GeomCutoffCallout"), once$layers
    )[[1]]$data,
    once_callout_data
  )
  expect_identical(
    unname(vapply(
      cutoff_test_layers(twice),
      function(layer) unique(layer$data$call_id), integer(1)
    )),
    1:2
  )
})

test_that("position transforms change drawing coordinates but not raw anchors", {
  values <- 10^(0:9)
  expected <- cutoff_test_expected("upper", .2, values)
  base <- ggplot2::ggplot(data.frame(x = values), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(bins = 10)
  plain <- suppressMessages(show_cutoffs(
    base, upper(x, .2), show_labels = TRUE
  ))
  logged <- suppressMessages(show_cutoffs(
    base + ggplot2::scale_x_log10(), upper(x, .2), show_labels = TRUE
  ))
  reversed <- suppressMessages(show_cutoffs(
    base + ggplot2::scale_x_reverse(), upper(x, .2), show_labels = TRUE
  ))

  for (plot in list(plain, logged, reversed)) {
    expect_equal(cutoff_test_layers(plot)[[1]]$data$xintercept, expected)
    expect_true(suppressWarnings(cutoff_test_draw(plot)))
  }
  expect_equal(
    as.numeric(ggplot2::layer_data(logged, length(logged$layers))$xintercept),
    expected
  )
  expect_equal(
    as.numeric(ggplot2::layer_data(reversed, length(reversed$layers))$xintercept),
    expected
  )
  expect_s3_class(ggplot2::layer_grob(logged, length(logged$layers))[[1]], "gTree")
  expect_s3_class(ggplot2::layer_grob(reversed, length(reversed$layers))[[1]], "gTree")
})

test_that("hard limits omit false boundary marks while zoom preserves raw anchors", {
  base <- cutoff_test_histogram(1:10)
  expected <- cutoff_test_expected("middle", .5, 1:10)
  censored <- suppressMessages(show_cutoffs(
    base + ggplot2::scale_x_continuous(limits = c(4, 7)),
    middle(x, .5), show_labels = TRUE
  ))
  squished <- suppressMessages(show_cutoffs(
    base + ggplot2::scale_x_continuous(
      limits = c(4, 7), oob = scales::squish
    ),
    middle(x, .5), show_labels = TRUE
  ))
  zoomed <- suppressMessages(show_cutoffs(
    base + ggplot2::coord_cartesian(xlim = c(4, 7)),
    middle(x, .5), show_labels = TRUE
  ))

  for (plot in list(censored, squished, zoomed)) {
    expect_equal(cutoff_test_layers(plot)[[1]]$data$xintercept, expected)
  }
  expect_equal(
    as.numeric(suppressWarnings(
      ggplot2::layer_data(censored, length(censored$layers))$xintercept
    )),
    expected
  )
  expect_equal(
    as.numeric(suppressWarnings(
      ggplot2::layer_data(squished, length(squished$layers))$xintercept
    )),
    expected
  )
  expect_s3_class(
    suppressWarnings(ggplot2::layer_grob(censored, length(censored$layers))[[1]]),
    "zeroGrob"
  )
  expect_s3_class(
    suppressWarnings(ggplot2::layer_grob(squished, length(squished$layers))[[1]]),
    "zeroGrob"
  )
  expect_equal(
    suppressWarnings(ggplot2::layer_data(zoomed, length(zoomed$layers))$xintercept),
    expected
  )
  expect_identical(zoomed$coordinates$limits$x, c(4, 7))
  expect_true(suppressWarnings(cutoff_test_draw(censored)))
  expect_true(suppressWarnings(cutoff_test_draw(squished)))
  expect_true(suppressWarnings(cutoff_test_draw(zoomed)))

  guide_warnings <- character()
  withCallingHandlers(
    ggplot2::ggplotGrob(squished),
    warning = function(condition) {
      guide_warnings <<- c(guide_warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  expect_no_match(guide_warnings, "Position guide")
})

test_that("show_cutoffs leaves caller guide choices under ggplot2 ownership", {
  base <- cutoff_test_histogram()
  caller <- ggplot2::guide_axis(angle = 17)
  composed <- suppressMessages(show_cutoffs(
    base + ggplot2::scale_x_continuous(guide = caller),
    middle(x, .5)
  ))
  composed_guide <- composed$scales$get_scales("x")$guide
  expect_s3_class(composed_guide, "GuideAxis")
  expect_identical(composed_guide$params$angle, 17)

  without_numeric_axis <- suppressMessages(show_cutoffs(
    base + ggplot2::guides(x = "none"), middle(x, .5)
  ))
  expect_identical(without_numeric_axis$guides$guides$x, "none")
  expect_no_error(ggplot2::ggplotGrob(without_numeric_axis))

  replaced <- suppressMessages(composed + ggplot2::scale_x_continuous())
  expect_s3_class(replaced$scales$get_scales("x")$guide, "waiver")

  suppressed_later <- composed + ggplot2::guides(x = "none")
  expect_identical(suppressed_later$guides$guides$x, "none")
  expect_no_error(ggplot2::ggplotGrob(suppressed_later))
})

test_that("coord_flip works before or after show_cutoffs", {
  base <- cutoff_test_histogram()
  before <- suppressMessages(show_cutoffs(
    base + ggplot2::coord_flip(), middle(x, .5), show_labels = TRUE
  ))
  after <- suppressMessages(show_cutoffs(
    base, middle(x, .5), show_labels = TRUE
  ) + ggplot2::coord_flip())

  expect_s3_class(before$coordinates, "CoordFlip")
  expect_s3_class(after$coordinates, "CoordFlip")
  expect_equal(
    cutoff_test_layers(before)[[1]]$data$xintercept,
    cutoff_test_layers(after)[[1]]$data$xintercept
  )
  expect_no_warning(ggplot2::ggplotGrob(before))
  expect_no_warning(ggplot2::ggplotGrob(after))
})

test_that("a top primary x guide remains the caller's ordinary axis", {
  out <- suppressMessages(show_cutoffs(
    cutoff_test_histogram() + ggplot2::scale_x_continuous(position = "top"),
    middle(x, .5), show_labels = TRUE
  ))
  scale <- out$scales$get_scales("x")

  expect_identical(scale$position, "top")
  expect_s3_class(scale$guide, "waiver")
  expect_true(suppressWarnings(cutoff_test_draw(out)))
})

test_that("show_cutoffs callout snapshot", {
  skip_if_not_installed("vdiffr")
  plot <- gf_histogram(
    ~Thumb, data = Fingers, fill = ~middle(Thumb, .95), bins = 30
  )

  suppressMessages(show_cutoffs(plot, show_labels = TRUE)) |>
    expect_doppelganger("show_cutoffs-middle-95")
})

test_that("show_cutoffs stacked callout snapshots", {
  skip_if_not_installed("vdiffr")
  plot <- suppressMessages(
    gf_histogram(~Thumb, data = Fingers, bins = 30) |>
      show_cutoffs(middle(Thumb, .999), show_labels = TRUE) |>
      show_cutoffs(
        middle(Thumb, .95), color = "firebrick", show_labels = TRUE
      ) |>
      show_cutoffs(
        middle(Thumb, .80), color = "darkgreen", show_labels = TRUE
      )
  )

  expect_doppelganger(plot, "show_cutoffs-stacked-three-levels")
  expect_doppelganger(
    plot + ggplot2::coord_flip(),
    "show_cutoffs-stacked-three-levels-flipped"
  )
})
