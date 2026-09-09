cutoff_draw <- function(plot, layer = length(plot$layers)) {
  built <- ggplot2::ggplot_build(plot)
  target <- built$plot$layers[[layer]]
  do.call(
    target$geom$draw_panel,
    c(
      list(
        data = built$data[[layer]],
        panel_params = built$layout$panel_params[[1]],
        coord = built$plot$coordinates
      ),
      target$geom_params
    )
  )
}

cutoff_test_solve <- function(data, horizontal = FALSE, height = 0.2,
                              avoidance = NULL) {
  measured <- measure_cutoff_callouts(data, 3.2, 1.6)
  solve_cutoff_callout_layout(
    measured$data, metrics = measured$metrics,
    panel_width = measured$panel_width,
    panel_height = measured$panel_height,
    horizontal = horizontal, height = height, avoidance = avoidance
  )
}

test_that("geom_cutoff has the conventional fixed-x geom interface", {
  expect_identical(
    names(formals(geom_cutoff)),
    c("mapping", "data", "stat", "position", "...", "height", "na.rm",
      "show.legend", "inherit.aes")
  )
  expect_identical(formals(geom_cutoff)$stat, "identity")
  expect_identical(formals(geom_cutoff)$position, "identity")
  expect_identical(formals(geom_cutoff)$height, 0.2)
  expect_identical(formals(geom_cutoff)$na.rm, FALSE)
  expect_identical(formals(geom_cutoff)$show.legend, NA)
  expect_identical(formals(geom_cutoff)$inherit.aes, TRUE)

  layer <- geom_cutoff(
    mapping = ggplot2::aes(xintercept = cutoff),
    data = data.frame(cutoff = 2), colour = "purple", linewidth = 2,
    show.legend = FALSE, inherit.aes = FALSE
  )
  expect_s3_class(layer, "LayerInstance")
  expect_s3_class(layer$geom, "GeomCutoff")
  expect_s3_class(layer$stat, "StatIdentity")
  expect_s3_class(layer$position, "PositionIdentity")
  expect_identical(layer$geom$required_aes, "xintercept")
  expect_identical(layer$geom$default_aes$linetype, "dashed")
  expect_identical(layer$geom_params$height, 0.2)
  expect_identical(layer$aes_params$colour, "purple")
  expect_identical(layer$aes_params$linewidth, 2)
  expect_identical(layer$show.legend, FALSE)
  expect_identical(layer$inherit.aes, FALSE)
})

test_that("cutoff stems use transformed anchors and panel-relative height", {
  base <- ggplot2::ggplot(data.frame(x = 1:5), ggplot2::aes(x)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = 0.5)
  plot <- base + geom_cutoff(
    ggplot2::aes(xintercept = cutoff),
    data = data.frame(cutoff = c(2, 4)), inherit.aes = FALSE,
    height = 0.2, colour = c("red", "blue"), linewidth = c(1, 2)
  )
  grob <- cutoff_draw(plot)

  expect_s3_class(grob, "segments")
  expect_identical(grid::unitType(grob$x0), rep("npc", 2))
  expect_identical(grid::unitType(grob$x1), rep("npc", 2))
  expect_identical(grid::unitType(grob$y0), rep("npc", 2))
  expect_identical(grid::unitType(grob$y1), rep("npc", 2))
  expect_equal(as.numeric(grob$x0), as.numeric(grob$x1))
  expect_true(all(as.numeric(grob$x0) > 0 & as.numeric(grob$x0) < 1))
  expect_lt(as.numeric(grob$x0)[[1]], as.numeric(grob$x0)[[2]])
  expect_true(all(as.numeric(grob$y0) > 0))
  expect_equal(as.numeric(grob$y1) - as.numeric(grob$y0), c(0.2, 0.2))
  expect_identical(grob$gp$col, c("#FF0000", "#0000FF"))
  expect_identical(grob$gp$lty, c("dashed", "dashed"))
})

test_that("coord_flip changes the stem orientation through its transform", {
  plot <- ggplot2::ggplot(data.frame(x = 1:5), ggplot2::aes(x)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = 0.5) +
    geom_cutoff(
      ggplot2::aes(xintercept = cutoff), data = data.frame(cutoff = 3),
      inherit.aes = FALSE, height = 0.35
    ) +
    ggplot2::coord_flip()
  grob <- cutoff_draw(plot)

  expect_identical(grid::unitType(grob$x0), "npc")
  expect_identical(grid::unitType(grob$x1), "npc")
  expect_identical(grid::unitType(grob$y0), "npc")
  expect_identical(grid::unitType(grob$y1), "npc")
  expect_gt(as.numeric(grob$x0), 0)
  expect_equal(as.numeric(grob$x1) - as.numeric(grob$x0), 0.35)
  expect_equal(as.numeric(grob$y0), as.numeric(grob$y1))
})

test_that("markers touch the axis and labelled cutoffs draw complete callouts", {
  base <- ggplot2::ggplot(data.frame(x = 1:10), ggplot2::aes(x)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = 0.5)
  plot <- suppressMessages(show_cutoffs(
    base, middle(x, .8), show_labels = TRUE
  ))
  stem_grob <- cutoff_draw(plot, layer = length(plot$layers) - 1L)
  callouts <- cutoff_draw(plot)

  expect_s3_class(stem_grob, "gTree")
  expect_s3_class(callouts, "coursekata_cutoff_callouts")
  polygons <- Filter(function(x) inherits(x, "polygon"), stem_grob$children)
  expect_equal(
    callouts$data$label,
    c(".1 of\nvalues below", ".1 of\nvalues above")
  )
  callouts <- grid::makeContent(callouts)
  leaders <- Filter(function(x) inherits(x, "polyline"), callouts$children)
  boxes <- Filter(function(x) inherits(x, "roundrect"), callouts$children)
  text <- Filter(function(x) inherits(x, "text"), callouts$children)

  expect_length(polygons, 2)
  expect_true(all(vapply(
    polygons, function(x) as.numeric(x$y[[3L]]) == 0, logical(1)
  )))
  expect_length(boxes, 2)
  expect_length(text, 2)
  expect_length(leaders, 2)
  expect_true(all(vapply(boxes, function(x) {
    !is.na(x$gp$fill) && substr(x$gp$fill, 8, 9) != "FF"
  }, logical(1))))
  expect_true(all(vapply(seq_along(boxes), function(i) {
    padding <- grid::convertWidth(
      grid::grobWidth(boxes[[i]]) - grid::grobWidth(text[[i]]),
      "mm", valueOnly = TRUE
    )
    isTRUE(all.equal(padding, 3.2, tolerance = 0.05))
  }, logical(1))))
})

test_that("callout labels reflow only when their measured boxes need it", {
  data <- data.frame(label = c(
    ".025 of\nvalues below", ".025 of\nvalues above"
  ))
  reflow_at <- function(width) {
    grid::pushViewport(grid::viewport(width = grid::unit(width, "mm")))
    on.exit(grid::popViewport())
    measure_cutoff_callouts(data, 3.2, 1.6)$data$label
  }

  expect_identical(reflow_at(80), data$label)
  expect_identical(
    reflow_at(30),
    c(".025 of\nvalues\nbelow", ".025 of\nvalues\nabove")
  )
})

test_that("callout drawing reuses one measurement pass", {
  base <- ggplot2::ggplot(data.frame(x = 1:10), ggplot2::aes(x)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = 0.5)
  plot <- suppressMessages(show_cutoffs(
    base, middle(x, .8), show_labels = TRUE
  ))
  callouts <- cutoff_draw(plot)
  metric_calls <- 0L
  original_metrics <- cutoff_callout_metrics
  local_mocked_bindings(
    cutoff_callout_metrics = function(...) {
      metric_calls <<- metric_calls + 1L
      original_metrics(...)
    },
    .package = "coursekata"
  )
  grid::pushViewport(grid::viewport(
    width = grid::unit(160, "mm"), height = grid::unit(110, "mm")
  ))
  on.exit(grid::popViewport())

  drawn <- grid::makeContent(callouts)

  expect_s3_class(drawn, "coursekata_cutoff_callouts")
  expect_identical(metric_calls, 1L)
})

test_that("nearest callout ports cover all four box sides", {
  box <- list(x1 = 40, x2 = 60, y1 = 40, y2 = 60, x = 50, y = 50)
  cases <- list(
    right = list(source = c(90, 50), point = c(60, 50), angle = 0),
    top = list(source = c(50, 90), point = c(50, 60), angle = 90),
    left = list(source = c(10, 50), point = c(40, 50), angle = 180),
    bottom = list(source = c(50, 10), point = c(50, 40), angle = 270)
  )

  for (name in names(cases)) {
    result <- cutoff_nearest_box_port(cases[[name]]$source, box)
    expect_equal(result$point, cases[[name]]$point, label = name)
    expect_identical(result$angle, cases[[name]]$angle, label = name)
  }
})

test_that("route geometry distinguishes clear, crossing, and shared paths", {
  box <- list(x1 = 40, x2 = 60, y1 = 40, y2 = 60, x = 50, y = 50)

  expect_true(cutoff_segment_hits_box(c(20, 50), c(80, 50), box))
  expect_true(cutoff_segment_hits_box(c(50, 20), c(50, 80), box))
  expect_false(cutoff_segment_hits_box(c(20, 20), c(80, 20), box))
  expect_false(cutoff_segment_hits_box(c(20, 20), c(80, 80), box))
  expect_identical(
    cutoff_route_box_hits(rbind(c(20, 20), c(20, 50), c(80, 50)), box),
    1L
  )

  crossing_horizontal <- rbind(c(10, 50), c(90, 50))
  crossing_vertical <- rbind(c(50, 10), c(50, 90))
  shared_first <- rbind(c(10, 30), c(70, 30))
  shared_second <- rbind(c(40, 30), c(90, 30))
  clear <- rbind(c(10, 80), c(90, 80))

  expect_identical(
    cutoff_route_conflict(crossing_horizontal, crossing_vertical), 120
  )
  expect_identical(cutoff_route_conflict(shared_first, shared_second), 1000)
  expect_identical(cutoff_route_conflict(shared_first, clear), 0)
})

test_that("box collision uses measured bounds and clearance", {
  first <- list(x1 = 10, x2 = 20, y1 = 10, y2 = 20)
  overlapping <- list(x1 = 19, x2 = 29, y1 = 19, y2 = 29)
  within_gap <- list(x1 = 21, x2 = 31, y1 = 10, y2 = 20)
  clear <- list(x1 = 22, x2 = 32, y1 = 10, y2 = 20)

  expect_true(cutoff_boxes_overlap(first, overlapping))
  expect_true(cutoff_boxes_overlap(first, within_gap))
  expect_false(cutoff_boxes_overlap(first, clear))
})

test_that("repeated callouts are jointly placed without box collisions", {
  data <- data.frame(
    label = rep(c(".025 of\nvalues below", ".025 of\nvalues above"), 3),
    call_id = rep(1:3, each = 2),
    side = rep(c("lower", "upper"), 3),
    x = 0, y = c(.20, .80, .25, .75, .30, .70),
    .screen = c(.20, .80, .25, .75, .30, .70),
    .opposite_x = 1, .opposite_y = c(.20, .80, .25, .75, .30, .70)
  )
  grid::pushViewport(grid::viewport(
    width = grid::unit(160, "mm"), height = grid::unit(110, "mm")
  ))
  on.exit(grid::popViewport())
  layout <- cutoff_test_solve(data, horizontal = TRUE)

  pairs <- utils::combn(seq_along(layout$boxes), 2L)
  expect_false(any(apply(pairs, 2L, function(pair) {
    cutoff_boxes_overlap(layout$boxes[[pair[[1L]]]], layout$boxes[[pair[[2L]]]])
  })))
  expect_true(all(vapply(seq_along(layout$routes), function(i) {
    route <- layout$routes[[i]]
    box <- layout$boxes[[i]]
    source <- cutoff_stem_source(
      data[i, , drop = FALSE], horizontal = TRUE, height = 0.2,
      panel_width = 160, panel_height = 110
    )
    attachment <- cutoff_nearest_box_port(source, box)
    isTRUE(all.equal(
      unname(route[nrow(route), ]), attachment$point
    )) && identical(layout$attachment_angles[[i]], attachment$angle)
  }, logical(1))))
  expect_true(all(vapply(seq_along(layout$routes), function(i) {
    other_boxes <- layout$boxes[setdiff(seq_along(layout$boxes), i)]
    all(vapply(
      other_boxes,
      function(box) cutoff_route_box_hits(layout$routes[[i]], box) == 0,
      logical(1)
    ))
  }, logical(1))))
  route_pairs <- utils::combn(seq_along(layout$routes), 2L)
  expect_equal(sum(apply(route_pairs, 2L, function(pair) {
    cutoff_route_conflict(
      layout$routes[[pair[[1L]]]], layout$routes[[pair[[2L]]]]
    )
  })), 0)
})

test_that("callout layout prefers whitespace without making it mandatory", {
  data <- data.frame(
    label = ".1 of\nvalues above", call_id = 1L,
    side = "upper", x = 0, y = .5, .screen = .5,
    .opposite_x = 1, .opposite_y = .5
  )
  occupied <- data.frame(
    axis = c(.4, .5, .6), baseline = 0, tip = .65
  )
  grid::pushViewport(grid::viewport(
    width = grid::unit(160, "mm"), height = grid::unit(110, "mm")
  ))
  on.exit(grid::popViewport())

  free <- cutoff_test_solve(data, horizontal = TRUE)
  prepare_calls <- 0L
  original_prepare <- prepare_cutoff_avoidance
  local_mocked_bindings(
    prepare_cutoff_avoidance = function(...) {
      prepare_calls <<- prepare_calls + 1L
      original_prepare(...)
    },
    .package = "coursekata"
  )
  avoiding <- cutoff_test_solve(
    data, horizontal = TRUE, avoidance = occupied
  )
  expect_identical(prepare_calls, 1L)
  expect_gt(avoiding$boxes[[1L]]$x, free$boxes[[1L]]$x)
})

test_that("callout search is bounded and deterministic", {
  data <- data.frame(
    label = rep(c(".025 of\nvalues below", ".025 of\nvalues above"), 3),
    call_id = rep(1:3, each = 2),
    side = rep(c("lower", "upper"), 3),
    x = c(.20, .80, .25, .75, .30, .70), y = 0,
    .screen = c(.20, .80, .25, .75, .30, .70),
    .opposite_x = c(.20, .80, .25, .75, .30, .70), .opposite_y = 1
  )
  grid::pushViewport(grid::viewport(
    width = grid::unit(160, "mm"), height = grid::unit(110, "mm")
  ))
  on.exit(grid::popViewport())

  measured <- measure_cutoff_callouts(data, 3.2, 1.6)
  metrics <- measured$metrics
  candidates <- cutoff_box_candidates(
    data[1, , drop = FALSE], metrics$width[[1]], metrics$height[[1]],
    "lower", FALSE, 0.2, 160, 110, nrow(data)
  )
  first <- solve_cutoff_callout_layout(
    measured$data, metrics = metrics,
    panel_width = measured$panel_width, panel_height = measured$panel_height
  )
  second <- solve_cutoff_callout_layout(
    measured$data, metrics = metrics,
    panel_width = measured$panel_width, panel_height = measured$panel_height
  )

  expect_lte(length(candidates), 48L)
  expect_named(
    first,
    c("boxes", "routes", "physical", "horizontal", "attachment_angles")
  )
  expect_false(any(c("axis", "depth", "cost") %in% names(first$boxes[[1L]])))
  expect_equal(first$boxes, second$boxes)
  expect_equal(first$routes, second$routes)
  expect_identical(first$attachment_angles, second$attachment_angles)
})

test_that("callout route search stays bounded as obstacles grow", {
  source <- c(20, 10)
  box <- list(x1 = 70, x2 = 90, y1 = 60, y2 = 76, x = 80, y = 68)
  obstacles <- lapply(seq_len(40L), function(i) {
    x <- 24 + (i %% 10L) * 6
    y <- 18 + (i %/% 10L) * 9
    list(x1 = x, x2 = x + 4, y1 = y, y2 = y + 5, x = x + 2, y = y + 2.5)
  })

  routes <- cutoff_route_candidates(
    source, box, panel_width = 120, panel_height = 90,
    obstacles = obstacles
  )

  expect_lte(length(routes), 90L)
  expect_true(all(vapply(routes, function(route) {
    isTRUE(all.equal(unname(route[1L, ]), source)) &&
      isTRUE(all.equal(
        unname(route[nrow(route), ]),
        cutoff_nearest_box_port(source, box)$point
      ))
  }, logical(1))))
})

test_that("cutoff stems neither train the count axis nor change clipping", {
  base <- ggplot2::ggplot(data.frame(x = 1:5), ggplot2::aes(x)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = 0.5) +
    ggplot2::scale_y_sqrt()
  out <- base + geom_cutoff(
    ggplot2::aes(xintercept = cutoff), data = data.frame(cutoff = 3),
    inherit.aes = FALSE
  )
  before <- ggplot2::ggplot_build(base)$layout$panel_params[[1]]$y.range
  after <- ggplot2::ggplot_build(out)$layout$panel_params[[1]]$y.range

  expect_equal(after, before)
  expect_identical(out$coordinates$clip, base$coordinates$clip)
  expect_s3_class(cutoff_draw(out), "segments")
})

test_that("height stays inside the panel", {
  mapping <- ggplot2::aes(xintercept = cutoff)
  data <- data.frame(cutoff = 1)

  for (height in list(-0.1, 1.1, Inf, c(0.1, 0.2), "short")) {
    plot <- ggplot2::ggplot() +
      geom_cutoff(mapping, data = data, height = height, inherit.aes = FALSE)
    expect_error(ggplot2::ggplot_build(plot), "one number from 0 to 1")
  }
  expect_no_error(ggplot2::ggplot_build(
    ggplot2::ggplot() +
      geom_cutoff(mapping, data = data, height = 0, inherit.aes = FALSE)
  ))
  expect_no_error(ggplot2::ggplot_build(
    ggplot2::ggplot() +
      geom_cutoff(mapping, data = data, height = 1, inherit.aes = FALSE)
  ))
})

protected_cutoff_plot <- function(oob) {
  ggplot2::ggplot(data.frame(x = 3:8), ggplot2::aes(x)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = 0.5) +
    geom_cutoff(
      ggplot2::aes(
        xintercept = xintercept, .value = .value,
        .coursekata_protect = protected
      ),
      data = data.frame(
        xintercept = c(1, 10), .value = c(1, 10), protected = TRUE
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::scale_x_continuous(limits = c(3, 8), oob = oob)
}

test_that("high-level protection omits censored and squished cutoff stems", {
  censored <- protected_cutoff_plot(scales::censor)
  squished <- protected_cutoff_plot(scales::squish)

  expect_s3_class(cutoff_draw(censored), "zeroGrob")
  expect_s3_class(cutoff_draw(squished), "zeroGrob")
})

test_that("high-level protection detects a custom OOB move", {
  move_inside <- function(x, range) {
    x[x < range[[1]]] <- range[[1]] + 0.25
    x[x > range[[2]]] <- range[[2]] - 0.25
    x
  }
  plot <- protected_cutoff_plot(move_inside)

  expect_s3_class(cutoff_draw(plot), "zeroGrob")
})

test_that("truthful transformed and reversed stems survive protection", {
  protected <- function(scale) {
    ggplot2::ggplot(data.frame(x = 1:10), ggplot2::aes(x)) +
      ggplot2::geom_histogram(binwidth = 1, boundary = 0.5) +
      geom_cutoff(
        ggplot2::aes(
          xintercept = xintercept, .value = .value,
          .coursekata_protect = protected
        ),
        data = data.frame(xintercept = c(2, 8), .value = c(2, 8), protected = TRUE),
        inherit.aes = FALSE
      ) +
      scale
  }

  expect_s3_class(cutoff_draw(protected(ggplot2::scale_x_log10())), "segments")
  expect_s3_class(cutoff_draw(protected(ggplot2::scale_x_reverse())), "segments")
  expect_s3_class(
    cutoff_draw(protected(ggplot2::scale_x_reverse()) + ggplot2::coord_flip()),
    "segments"
  )
})

test_that("high-level stems begin at a top guide and follow it through a flip", {
  base <- ggplot2::ggplot(data.frame(x = 1:10), ggplot2::aes(x)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = 0.5) +
    ggplot2::scale_x_continuous(position = "top")
  upright <- suppressMessages(show_cutoffs(base, middle(x, .5)))
  flipped <- suppressMessages(show_cutoffs(
    base + ggplot2::coord_flip(), middle(x, .5)
  ))
  upright_grob <- cutoff_draw(upright)
  flipped_grob <- cutoff_draw(flipped)
  upright_stems <- upright_grob$children[[1L]]
  flipped_stems <- flipped_grob$children[[1L]]
  upright_markers <- Filter(
    function(grob) inherits(grob, "polygon"), upright_grob$children
  )
  flipped_markers <- Filter(
    function(grob) inherits(grob, "polygon"), flipped_grob$children
  )

  expect_equal(as.numeric(upright_stems$y0), c(1, 1))
  expect_equal(as.numeric(upright_stems$y1), c(0.8, 0.8))
  expect_equal(as.numeric(flipped_stems$x0), c(1, 1))
  expect_equal(as.numeric(flipped_stems$x1), c(0.8, 0.8))
  expect_true(all(vapply(
    upright_markers, function(grob) as.numeric(grob$y[[3L]]) == 1, logical(1)
  )))
  expect_true(all(vapply(
    flipped_markers, function(grob) as.numeric(grob$x[[3L]]) == 1, logical(1)
  )))
})

test_that("a later non-cartesian coordinate drops protected stems cleanly", {
  base <- ggplot2::ggplot(data.frame(x = 1:10), ggplot2::aes(x)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = 0.5)
  marked <- suppressMessages(show_cutoffs(base, middle(x, .5)))
  polar <- marked + ggplot2::coord_polar()

  expect_no_error(ggplot2::ggplotGrob(polar))
  expect_s3_class(ggplot2::layer_grob(polar, length(polar$layers))[[1]], "zeroGrob")
})
