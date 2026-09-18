annotation_histogram <- function() {
  gf_histogram(
    ~Thumb, data = Fingers, bins = 30, fill = ~middle(Thumb, .95)
  )
}

primary_guide_children <- function(plot) {
  guide <- plot$guides$guides$x
  if (is.null(guide)) {
    scale <- plot$scales$get_scales("x")
    if (is.null(scale)) return(list())
    guide <- scale$guide
  }
  if (inherits(guide, "GuideAxisStack")) guide$params$guides else list(guide)
}

coursekata_child_signature <- function(plot) {
  vapply(primary_guide_children(plot), function(guide) {
    if (inherits(guide, "GuideCutoff")) {
      return(paste0("cutoff-", guide$params$call_id))
    }
    if (inherits(guide, "GuideDgp")) {
      return(paste0("dgp-", guide$params$role))
    }
    "caller"
  }, character(1))
}

test_that("cutoff calls keep stable layer identity without changing the axis", {
  base <- annotation_histogram() +
    ggplot2::scale_x_continuous(guide = ggplot2::guide_axis(angle = 17))
  marked <- suppressMessages(show_cutoffs(base, show_labels = TRUE))
  marked <- suppressMessages(show_cutoffs(
    marked, middle(Thumb, .99), show_labels = TRUE, color = "firebrick"
  ))
  marked <- suppressMessages(show_cutoffs(
    marked, middle(Thumb, .80), show_labels = TRUE, color = "darkgreen"
  ))

  expect_identical(
    coursekata_child_signature(marked),
    "caller"
  )
  cutoff <- Filter(
    function(layer) inherits(layer$geom, "GeomCutoff"), marked$layers
  )
  expect_identical(
    unname(vapply(
      cutoff, function(x) unique(x$data$call_id), integer(1)
    )),
    1:3
  )
  expect_identical(
    unname(vapply(cutoff, function(x) x$aes_params$colour, character(1))),
    c("#1e3a8a", "firebrick", "darkgreen")
  )
  expect_no_error(ggplot2::ggplotGrob(marked))
})

test_that("cutoff and DGP guide order is independent of helper order", {
  base <- annotation_histogram()
  cutoff_then_dgp <- suppressMessages(
    show_dgp(show_cutoffs(base, middle(Thumb, .99), show_labels = TRUE))
  )
  dgp_then_cutoff <- suppressMessages(
    show_cutoffs(show_dgp(base), middle(Thumb, .99), show_labels = TRUE)
  )

  expected <- c("caller", "dgp-estimate")
  expect_identical(coursekata_child_signature(cutoff_then_dgp), expected)
  expect_identical(coursekata_child_signature(dgp_then_cutoff), expected)
  expect_length(
    position_guide_matches(
      cutoff_then_dgp$guides$guides$x.sec, "GuideDgp", "population"
    ),
    1
  )
  expect_length(
    position_guide_matches(
      dgp_then_cutoff$guides$guides$x.sec, "GuideDgp", "population"
    ),
    1
  )
  expect_no_error(ggplot2::ggplotGrob(cutoff_then_dgp))
  expect_no_error(ggplot2::ggplotGrob(dgp_then_cutoff))
})

test_that("mean, DGP, and cutoff helpers retain their separate meanings", {
  base <- annotation_histogram()
  mean_first <- suppressMessages(
    base |>
      show_mean() |>
      show_dgp() |>
      show_cutoffs(show_labels = TRUE)
  )
  cutoff_first <- suppressMessages(
    base |>
      show_cutoffs(show_labels = TRUE) |>
      show_dgp() |>
      show_mean()
  )

  for (plot in list(mean_first, cutoff_first)) {
    mean_layer <- which(vapply(plot$layers, function(layer) {
      identical(attr(layer, "coursekata_layer"), "distribution_mean")
    }, logical(1)))
    cutoff_layers <- Filter(
      function(layer) inherits(layer$geom, "GeomCutoff"), plot$layers
    )
    callout_layers <- Filter(
      function(layer) inherits(layer$geom, "GeomCutoffCallout"), plot$layers
    )
    expect_length(mean_layer, 1L)
    expect_equal(
      ggplot2::layer_data(plot, mean_layer)$xintercept,
      mean(Fingers$Thumb)
    )
    expect_length(cutoff_layers, 1L)
    expect_equal(
      cutoff_layers[[1L]]$data$xintercept,
      unlist(cutoff_plan(
        list(func = "middle", prop = .95, greedy = TRUE), Fingers$Thumb
      )[c("lower", "upper")], use.names = FALSE)
    )
    expect_length(callout_layers, 1L)
    expect_length(
      position_guide_matches(plot$guides$guides$x, "GuideDgp", "estimate"), 1L
    )
    expect_length(
      position_guide_matches(
        plot$guides$guides$x.sec, "GuideDgp", "population"
      ),
      1L
    )
    expect_no_error(grid::grid.force(ggplot2::ggplotGrob(plot)))
  }
})

test_that("cutoff layers follow coord_flip whether added before or after", {
  base <- annotation_histogram()
  flip_first <- suppressMessages(show_cutoffs(
    base + ggplot2::coord_flip(), show_labels = TRUE
  ))
  flip_later <- suppressMessages(
    show_cutoffs(base, show_labels = TRUE) + ggplot2::coord_flip()
  )

  expect_no_error(first_grob <- ggplot2::ggplotGrob(flip_first))
  expect_no_error(later_grob <- ggplot2::ggplotGrob(flip_later))
  expect_true("axis-l" %in% first_grob$layout$name)
  expect_true("axis-l" %in% later_grob$layout$name)
  expect_length(coursekata_child_signature(flip_first), 0)
  expect_length(coursekata_child_signature(flip_later), 0)
})

test_that("later guide suppression and scale replacement use ggplot2 ownership", {
  base <- annotation_histogram()
  upright <- suppressMessages(show_cutoffs(base, show_labels = TRUE))
  flipped <- suppressMessages(show_cutoffs(
    base + ggplot2::coord_flip(), show_labels = TRUE
  ))

  expect_no_error(ggplot2::ggplotGrob(upright + ggplot2::guides(x = "none")))
  expect_no_error(ggplot2::ggplotGrob(flipped + ggplot2::guides(y = "none")))

  replaced <- suppressMessages(upright + ggplot2::scale_x_continuous())
  expect_s3_class(replaced$scales$get_scales("x")$guide, "waiver")
  expect_length(
    position_guide_matches(
      replaced$scales$get_scales("x")$guide, "GuideCutoff"
    ),
    0
  )
})

test_that("cutoff composition supports a top position guide", {
  base <- annotation_histogram() +
    ggplot2::scale_x_continuous(position = "top")
  marked <- suppressMessages(show_cutoffs(base, show_labels = TRUE))

  expect_no_error(grob <- ggplot2::ggplotGrob(marked))
  expect_true("axis-t" %in% grob$layout$name)
  expect_identical(coursekata_child_signature(marked), "caller")
})

test_that("all helper orders preserve statistics, anchors, and guide data", {
  values <- data.frame(x = c(-4, -2, -1, 1, 3, 6))
  base <- ggplot2::ggplot(values, ggplot2::aes(x)) +
    ggplot2::geom_histogram(binwidth = 1)
  helpers <- list(
    mean = show_mean,
    cutoff = function(plot) show_cutoffs(plot, middle(x, .5), show_labels = TRUE),
    dgp = show_dgp
  )
  orders <- list(
    c("mean", "cutoff", "dgp"), c("mean", "dgp", "cutoff"),
    c("cutoff", "mean", "dgp"), c("cutoff", "dgp", "mean"),
    c("dgp", "mean", "cutoff"), c("dgp", "cutoff", "mean")
  )
  plots <- lapply(orders, function(order) {
    Reduce(function(plot, name) helpers[[name]](plot), order, init = base)
  })
  expected <- cutoff_plan(
    list(func = "middle", prop = .5, greedy = TRUE), values$x
  )
  expected <- unlist(expected[c("lower", "upper")], use.names = FALSE)
  ranges <- ggplot2::ggplot_build(base)$layout$panel_params[[1L]]

  for (plot in plots) {
    built <- ggplot2::ggplot_build(plot)
    expect_equal(
      built$data[[layer_index(plot, "distribution_mean")]]$xintercept,
      mean(values$x)
    )
    expect_equal(as.numeric(
      built$data[[layer_index(plot, "distribution_cutoff")]]$xintercept
    ), expected)
    expect_length(layer_indices(plot, "distribution_cutoff_callouts"), 1L)
    expect_equal(built$layout$panel_params[[1L]]$x.range, ranges$x.range)
    expect_equal(built$layout$panel_params[[1L]]$y.range, ranges$y.range)
    population <- ggplot2::get_guide_data(plot, "x.sec")
    expect_equal(nrow(population), 1L)
    expect_identical(population$role, "population")
    expect_equal(population$.value, 0)
    expect_equal(population$x, (0 - ranges$x.range[[1L]]) / diff(ranges$x.range))
    # get_guide_data() has no key for an axis stack. Its trained children
    # retain their own keys, including the estimate's separate null anchor.
    trained <- built$layout$panel_params[[1L]]$guides$get_params("x")$guide_params
    expect_length(trained, 2L)
    estimate <- trained[[2L]]$key
    expect_equal(nrow(estimate), 1L)
    expect_identical(estimate$role, "estimate")
    expect_equal(estimate$.value, 0)
    expect_equal(estimate$x, population$x)
    expect_no_error(grid::grid.force(ggplot2::ggplotGrob(plot)))
  }
})

test_that("DGP defaults preserve caller labels and explicit axis styling", {
  caller_theme <- ggplot2::theme(
    axis.line.x = ggplot2::element_line(colour = "orange", linewidth = 1.2),
    axis.line.y = ggplot2::element_line(colour = "purple", linewidth = 1.3),
    axis.text.x = ggplot2::element_text(colour = "brown", face = "italic"),
    axis.title.x = ggplot2::element_text(colour = "forestgreen", face = "italic")
  )
  base <- annotation_histogram() + ggplot2::labs(x = "Shuffled slope (b1)")
  before <- show_dgp(base + caller_theme)
  after <- show_dgp(base) + caller_theme
  for (plot in list(before, after)) {
    expect_identical(plot$labels$x, "Shuffled slope (b1)")
    for (name in names(caller_theme)) {
      expect_equal(plot$theme[[name]], caller_theme[[name]])
    }
    replaced <- plot + ggplot2::guides(x = ggplot2::guide_axis())
    expect_identical(ggplot2::get_labs(replaced)$x, "Shuffled slope (b1)")
    expect_no_error(ggplot2::ggplotGrob(replaced))
  }
  expect_equal(base$theme, ggplot2::theme())
})

test_that("DGP defaults preserve existing inherited axis theme settings", {
  inherited <- ggplot2::theme(
    axis.text = ggplot2::element_text(colour = "purple", face = "italic"),
    axis.title = ggplot2::element_text(colour = "orange", face = "italic"),
    axis.line = ggplot2::element_line(colour = "forestgreen", linewidth = 1.2)
  )
  base <- annotation_histogram() + inherited
  marked <- show_dgp(base)
  for (name in c("axis.text.x", "axis.title.x", "axis.line.x", "axis.line.y")) {
    expect_equal(
      ggplot2::calc_element(name, marked$theme),
      ggplot2::calc_element(name, base$theme)
    )
  }
  expect_no_error(ggplot2::ggplotGrob(marked))

  partial <- show_dgp(annotation_histogram() + ggplot2::theme(
    axis.text = ggplot2::element_text(face = "italic")
  ))
  text <- ggplot2::calc_element("axis.text.x", partial$theme)
  expect_identical(text$face, "italic")
  expect_identical(text$colour, "#003d70")

  blank <- show_dgp(annotation_histogram() + ggplot2::theme(
    axis.text = ggplot2::element_blank(), axis.line = ggplot2::element_blank()
  ))
  expect_s3_class(ggplot2::calc_element("axis.text.x", blank$theme), "element_blank")
  expect_s3_class(ggplot2::calc_element("axis.line.x", blank$theme), "element_blank")
  expect_no_error(ggplot2::ggplotGrob(blank))

  # Later parent-only changes cannot override a more specific x/y element;
  # this is ordinary ggplot2 precedence, also used by the helper's defaults.
  later <- show_dgp(annotation_histogram()) + inherited
  expect_identical(ggplot2::calc_element("axis.text.x", later$theme)$colour, "#003d70")
  expect_identical(ggplot2::calc_element("axis.title.x", later$theme)$colour, "#003d70")
  expect_s3_class(ggplot2::calc_element("axis.line.y", later$theme), "element_blank")
})

test_that("DGP styling applies inherited relative dimensions only once", {
  styles <- list(
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = ggplot2::rel(.8)),
      axis.title = ggplot2::element_text(size = ggplot2::rel(1.2)),
      axis.line = ggplot2::element_line(linewidth = ggplot2::rel(1.5))
    ),
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = ggplot2::rel(.8), colour = "purple"),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(.5)),
      axis.title = ggplot2::element_text(size = ggplot2::rel(1.2), colour = "orange"),
      axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.5))
    )
  )
  for (style in styles) {
    base <- annotation_histogram() + style
    marked <- show_dgp(base)
    before <- ggplot2::theme_get() + base$theme
    after <- ggplot2::theme_get() + marked$theme
    for (name in c("axis.text.x", "axis.title.x")) {
      expect_equal(
        ggplot2::calc_element(name, after)$size,
        ggplot2::calc_element(name, before)$size
      )
    }
    expect_equal(marked$theme$axis.text$size, style$axis.text$size)
    expect_equal(marked$theme$axis.title$size, style$axis.title$size)
    if (!is.null(style$axis.line)) {
      expect_equal(
        ggplot2::calc_element("axis.line.x", after)$linewidth,
        ggplot2::calc_element("axis.line.x", before)$linewidth
      )
    }
    expect_no_error(ggplot2::ggplotGrob(marked))
  }
})

test_that("DGP duplicate detection follows effective native guide overrides", {
  base <- annotation_histogram() + ggplot2::scale_x_continuous(
    guide = guide_dgp(role = "estimate")
  )
  expect_error(show_dgp(base), "already")
  for (replacement in list(ggplot2::guide_axis(), "none", NULL)) {
    overridden <- base + ggplot2::guides(x = replacement)
    expect_no_error(marked <- show_dgp(overridden))
    expect_length(position_guide_matches(
      marked$guides$guides$x, "GuideDgp", "estimate"
    ), 1L)
    expect_no_error(ggplot2::ggplotGrob(marked))
  }
  unrelated <- annotation_histogram() + ggplot2::guides(y = guide_dgp())
  expect_no_error(show_dgp(unrelated))
})

test_that("native cutoff guides retain their order and styling in a DGP stack", {
  caller <- ggplot2::guide_axis_stack(
    ggplot2::guide_axis(angle = 17),
    guide_cutoff(c(45, 75), call_id = 2L, colour = "purple"),
    guide_cutoff(c(50, 70), call_id = 1L, colour = "orange"),
    spacing = grid::unit(2, "mm")
  )
  plot <- show_dgp(annotation_histogram() + ggplot2::guides(x = caller))
  expect_identical(coursekata_child_signature(plot),
                   c("caller", "cutoff-1", "cutoff-2", "dgp-estimate"))
  children <- primary_guide_children(plot)
  expect_identical(children[[1L]]$params$angle, 17)
  expect_identical(children[[2L]]$params$colour, "orange")
  expect_identical(children[[3L]]$params$colour, "purple")
  expect_equal(plot$guides$guides$x$params$spacing, grid::unit(2, "mm"))
  expect_length(caller$params$guides, 3L)
  expect_no_error(grid::grid.force(ggplot2::ggplotGrob(plot)))
})

test_that("DGP teaching sides are validated before and after scale changes", {
  base <- annotation_histogram()
  expect_error(
    show_dgp(base + ggplot2::scale_x_continuous(position = "top")),
    "primary x guide at the bottom"
  )
  later <- suppressMessages(
    show_dgp(base) + ggplot2::scale_x_continuous(position = "top")
  )
  expect_error(ggplot2::ggplotGrob(later), "primary x guide at the bottom")
  for (replacement in list("none", NULL, ggplot2::guide_axis())) {
    replaced <- later + ggplot2::guides(x = replacement)
    expect_error(ggplot2::ggplotGrob(replaced), "population guide at the top")
  }
  expect_no_error(ggplot2::ggplotGrob(
    show_dgp(base) + ggplot2::guides(x = "none")
  ))
})
