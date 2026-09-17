cutoff_layer_data <- function(plot) {
  ggplot2::layer_data(plot, length(plot$layers))
}

planned_intercepts <- function(part, prop, values, greedy = TRUE) {
  plan <- cutoff_plan(list(func = part, prop = prop, greedy = greedy), values)
  out <- c(plan$lower, plan$upper)
  out[!is.na(out)]
}

test_that("stat_cutoff has the conventional layer interface", {
  expect_identical(
    names(formals(stat_cutoff)),
    c("mapping", "data", "geom", "position", "...", "part", "prop",
      "greedy", "func", "na.rm", "show.legend", "inherit.aes")
  )
  expect_identical(formals(stat_cutoff)$geom, "cutoff")
  expect_identical(formals(stat_cutoff)$position, "identity")
  expect_identical(formals(stat_cutoff)$part, "middle")
  expect_identical(formals(stat_cutoff)$prop, 0.95)
  expect_identical(formals(stat_cutoff)$greedy, TRUE)
  expect_identical(deparse1(formals(stat_cutoff)$func), "lifecycle::deprecated()")
  expect_identical(formals(stat_cutoff)$na.rm, FALSE)
  expect_identical(formals(stat_cutoff)$show.legend, NA)
  expect_identical(formals(stat_cutoff)$inherit.aes, TRUE)

  default_layer <- stat_cutoff()
  expect_s3_class(default_layer$stat, "StatCutoff")
  expect_s3_class(default_layer$geom, "GeomCutoff")

  values <- data.frame(value = 1:10)
  layer <- stat_cutoff(
    mapping = ggplot2::aes(x = value), data = values, geom = "vline",
    part = "upper", prop = .2, greedy = FALSE, colour = "purple",
    na.rm = TRUE, show.legend = FALSE, inherit.aes = FALSE
  )

  expect_s3_class(layer, "LayerInstance")
  expect_s3_class(layer$stat, "StatCutoff")
  expect_s3_class(layer$geom, "GeomVline")
  expect_s3_class(layer$position, "PositionIdentity")
  expect_identical(layer$stat_params$part, "upper")
  expect_identical(layer$stat_params$prop, .2)
  expect_identical(layer$stat_params$greedy, FALSE)
  expect_identical(layer$stat_params$na.rm, TRUE)
  expect_identical(layer$aes_params$colour, "purple")
  expect_identical(layer$show.legend, FALSE)
  expect_identical(layer$inherit.aes, FALSE)
})

test_that("stat_cutoff delegates all five parts to cutoff_plan", {
  values <- c(1:20, NA_real_)
  cases <- list(
    middle = .8, tails = .8, outer = .2, upper = .2, lower = .2
  )

  for (part in names(cases)) {
    prop <- cases[[part]]
    plot <- ggplot2::ggplot(data.frame(x = values), ggplot2::aes(x = x)) +
      stat_cutoff(geom = "vline", part = part, prop = prop, na.rm = TRUE)

    expect_equal(
      sort(cutoff_layer_data(plot)$xintercept),
      sort(planned_intercepts(part, prop, values)),
      label = part
    )
  }
})

test_that("stat_cutoff preserves greedy, tie, missing-value, and endpoint behavior", {
  cases <- list(
    list(values = 1:10, part = "upper", prop = .25, greedy = TRUE),
    list(values = 1:10, part = "upper", prop = .25, greedy = FALSE),
    list(values = c(1, 1, 2, 2, 3, 3, 4, 4, NA),
         part = "middle", prop = .5, greedy = TRUE),
    list(values = 1:10, part = "middle", prop = 0, greedy = TRUE),
    list(values = 1:10, part = "middle", prop = 1, greedy = TRUE),
    list(values = 1:10, part = "lower", prop = 0, greedy = FALSE),
    list(values = 1:10, part = "upper", prop = 1, greedy = FALSE)
  )

  for (case in cases) {
    plot <- ggplot2::ggplot(data.frame(x = case$values), ggplot2::aes(x = x)) +
      stat_cutoff(
        geom = "vline", part = case$part, prop = case$prop,
        greedy = case$greedy, na.rm = TRUE
      )
    expected <- planned_intercepts(
      case$part, case$prop, case$values, case$greedy
    )

    expect_equal(sort(cutoff_layer_data(plot)$xintercept), sort(expected))
  }
})

test_that("stat_cutoff computes independently within each facet panel", {
  values <- data.frame(
    x = c(1:10, 101:120),
    group = rep(c("small", "large"), c(10, 20))
  )
  plot <- ggplot2::ggplot(values, ggplot2::aes(x = x)) +
    stat_cutoff(geom = "vline", part = "middle", prop = .5, na.rm = TRUE) +
    ggplot2::facet_wrap(~group)
  built <- cutoff_layer_data(plot)
  observed <- lapply(split(built$xintercept, built$PANEL), sort)
  expected <- list(
    sort(planned_intercepts("middle", .5, values$x[values$group == "large"])),
    sort(planned_intercepts("middle", .5, values$x[values$group == "small"]))
  )

  expect_equal(unname(observed), expected)
})

test_that("stat_cutoff preserves the inverse-select-forward transform round trip", {
  values <- data.frame(x = 10^(0:9))
  raw <- planned_intercepts("upper", .2, values$x)
  base <- ggplot2::ggplot(values, ggplot2::aes(x = x)) +
    stat_cutoff(geom = "vline", part = "upper", prop = .2, na.rm = TRUE)

  expect_equal(
    cutoff_layer_data(base + ggplot2::scale_x_log10())$xintercept,
    log10(raw)
  )
  expect_equal(
    cutoff_layer_data(base + ggplot2::scale_x_reverse())$xintercept,
    -raw
  )
})

test_that("hard scale limits change the panel input but coordinate zoom does not", {
  values <- data.frame(x = 1:10)
  base <- ggplot2::ggplot(values, ggplot2::aes(x = x)) +
    stat_cutoff(geom = "vline", part = "middle", prop = .5, na.rm = TRUE)

  limited <- cutoff_layer_data(
    base + ggplot2::scale_x_continuous(limits = c(1, 8))
  )$xintercept
  zoomed <- cutoff_layer_data(
    base + ggplot2::coord_cartesian(xlim = c(1, 8))
  )$xintercept

  expect_equal(sort(limited), sort(planned_intercepts("middle", .5, 1:8)))
  expect_equal(sort(zoomed), sort(planned_intercepts("middle", .5, 1:10)))
})

test_that("stat_cutoff refuses mapped styling aesthetics", {
  values <- data.frame(x = 1:6, g = rep(c("a", "b"), each = 3))
  plot <- ggplot2::ggplot(values, ggplot2::aes(x, colour = g)) +
    stat_cutoff(geom = "vline", na.rm = TRUE)

  expect_error(ggplot2::ggplot_build(plot), "colour.*can't be mapped")
})

test_that("func remains a deprecated alias at both public and direct-layer boundaries", {
  values <- data.frame(x = 1:10)
  cache <- get("deprecation_env", asNamespace("lifecycle"))
  clear_deprecation <- function() {
    id <- "coursekata-stat-cutoff-func"
    if (exists(id, envir = cache, inherits = FALSE)) rlang::env_unbind(cache, id)
  }
  clear_deprecation()
  withr::defer(clear_deprecation())

  lifecycle::expect_deprecated(
    aliased <- stat_cutoff(
      mapping = ggplot2::aes(x = x), data = values, geom = "vline",
      func = "upper", prop = .2, na.rm = TRUE
    ),
    "func"
  )
  expect_equal(
    cutoff_layer_data(ggplot2::ggplot() + aliased)$xintercept,
    planned_intercepts("upper", .2, values$x)
  )

  direct <- ggplot2::layer(
    stat = StatCutoff, geom = ggplot2::GeomVline, position = "identity",
    mapping = ggplot2::aes(x = x), data = values,
    params = list(func = "lower", prop = .2, na.rm = TRUE)
  )
  clear_deprecation()
  lifecycle::expect_deprecated(
    built <- cutoff_layer_data(ggplot2::ggplot() + direct),
    "func"
  )
  expect_equal(built$xintercept, planned_intercepts("lower", .2, values$x))

  expect_error(
    stat_cutoff(geom = "vline", part = "middle", func = "tails"),
    "both `part` and deprecated `func`"
  )
})

test_that("stat_cutoff refuses invalid parts and proportions by its public name", {
  values <- data.frame(x = 1:10)
  marked <- function(...) {
    ggplot2::ggplot_build(
      ggplot2::ggplot(values, ggplot2::aes(x = x)) +
        stat_cutoff(geom = "vline", na.rm = TRUE, ...)
    )
  }

  expect_error(marked(part = "bogus"), "stat_cutoff.*part")
  expect_error(marked(part = 42), "stat_cutoff.*part")
  expect_error(marked(prop = -0.1), "stat_cutoff.*prop")
  expect_error(marked(prop = 1.1), "stat_cutoff.*prop")
  expect_error(marked(prop = c(.1, .2)), "stat_cutoff.*prop")
})
