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
