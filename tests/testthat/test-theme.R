theme_test_raw_geom_defaults <- function() {
  geoms <- names(coursekata_theme_geom_defaults())
  stats::setNames(lapply(geoms, function(geom) {
    current <- ggplot2::update_geom_defaults(
      geom, ggplot2::aes(alpha = 0.123456789)
    )
    ggplot2::update_geom_defaults(geom, current)
    current
  }), geoms)
}

local_theme_lifecycle_state <- function(.local_envir = parent.frame()) {
  was_active <- isTRUE(.coursekata_theme_state$active)
  if (was_active) {
    original <- .coursekata_theme_state$snapshot
    coursekata_unload_theme()
  } else {
    coursekata_load_theme()
    original <- .coursekata_theme_state$snapshot
    coursekata_unload_theme()
  }

  withr::defer({
    coursekata_unload_theme()
    for (geom in names(original$geom_defaults)) {
      ggplot2::update_geom_defaults(geom, original$geom_defaults[[geom]])
    }
    ggplot2::theme_set(original$theme)
    restore_coursekata_options(original$options)
    .coursekata_theme_state$active <- FALSE
    .coursekata_theme_state$snapshot <- NULL
    if (was_active) coursekata_load_theme()
  }, envir = .local_envir)
}

test_that("loading applies the CourseKata plotting defaults", {
  local_theme_lifecycle_state()

  expect_invisible(coursekata_load_theme())
  expect_identical(ggplot2::theme_get(), theme_coursekata())
  expect_identical(getOption("repr.plot.width"), 6)
  expect_identical(getOption("repr.plot.height"), 4)
  expect_null(getOption("ggplot2.discrete.fill"))
  expect_null(getOption("ggplot2.discrete.colour"))
  expect_equal(
    ggplot2::theme_get()$palette.fill.discrete(3), coursekata_palette_provider()(3)
  )
  expect_identical(getOption("ggplot2.continuous.colour"), "viridis")

  expect_identical(ggplot2::get_geom_defaults("bar")$linewidth, 0.1)
  expect_identical(ggplot2::get_geom_defaults("point")$size, 2)
  expect_identical(
    ggplot2::get_geom_defaults("vline")$colour,
    coursekata_palette("blue80")
  )
})

test_that("unloading restores the caller's exact plotting state", {
  local_theme_lifecycle_state()

  caller_theme <- ggplot2::theme_minimal() + ggplot2::theme(
    plot.title = ggplot2::element_text(colour = "purple")
  )
  ggplot2::theme_set(caller_theme)
  restore_coursekata_options(list(
    repr.plot.width = 11,
    ggplot2.discrete.fill = identity,
    ggplot2.continuous.colour = "magma"
  ))
  ggplot2::update_geom_defaults("bar", ggplot2::aes(
    fill = "orange", linewidth = 0.75
  ))
  ggplot2::update_geom_defaults("point", ggplot2::aes(
    shape = 8, colour = "navy", size = 7, alpha = 0.25
  ))

  caller_options <- coursekata_theme_option_state()
  caller_defaults <- lapply(
    names(coursekata_theme_geom_defaults()), ggplot2::get_geom_defaults
  )
  caller_raw_defaults <- theme_test_raw_geom_defaults()

  coursekata_load_theme()
  ggplot2::theme_set(ggplot2::theme_void())
  options(repr.plot.width = 99)
  ggplot2::update_geom_defaults("point", ggplot2::aes(size = 99))
  coursekata_load_theme()
  expect_identical(ggplot2::theme_get(), theme_coursekata())
  expect_identical(ggplot2::get_geom_defaults("point")$size, 2)

  expect_invisible(coursekata_unload_theme())
  expect_identical(ggplot2::theme_get(), caller_theme)
  expect_identical(coursekata_theme_option_state(), caller_options)
  expect_identical(
    lapply(names(coursekata_theme_geom_defaults()), ggplot2::get_geom_defaults),
    caller_defaults
  )
  expect_identical(theme_test_raw_geom_defaults(), caller_raw_defaults)

  restored <- list(
    theme = ggplot2::theme_get(),
    options = coursekata_theme_option_state(),
    defaults = theme_test_raw_geom_defaults()
  )
  expect_invisible(coursekata_unload_theme())
  expect_identical(ggplot2::theme_get(), restored$theme)
  expect_identical(coursekata_theme_option_state(), restored$options)
  expect_identical(theme_test_raw_geom_defaults(), restored$defaults)
})

test_that("a new load cycle snapshots a new caller state", {
  local_theme_lifecycle_state()

  ggplot2::theme_set(ggplot2::theme_classic())
  options(repr.plot.height = 9)
  coursekata_load_theme()
  coursekata_unload_theme()

  expect_identical(ggplot2::theme_get(), ggplot2::theme_classic())
  expect_identical(getOption("repr.plot.height"), 9)

  next_theme <- ggplot2::theme_void()
  ggplot2::theme_set(next_theme)
  options(repr.plot.height = 13)
  coursekata_load_theme()
  coursekata_unload_theme()

  expect_identical(ggplot2::theme_get(), next_theme)
  expect_identical(getOption("repr.plot.height"), 13)
})

test_that("theme palettes allow per-plot overrides and explicit shared scales", {
  local_theme_lifecycle_state()
  coursekata_load_theme()
  data <- data.frame(x = 1:4, group = factor(c("a", "b", "a", "b")))
  base <- ggplot2::ggplot(data, ggplot2::aes(x, x, colour = group, fill = group)) +
    ggplot2::geom_point(shape = 21)
  drawn <- ggplot2::ggplot_build(base)$data[[1]]
  expected <- coursekata_palette_provider()(2)[as.integer(data$group)]
  expect_equal(drawn$colour, expected)
  expect_equal(drawn$fill, expected)

  custom <- function(n) c("red", "blue")[seq_len(n)]
  expect_no_warning(
    overridden <- ggplot2::ggplot_build(base + ggplot2::theme(
      palette.colour.discrete = custom, palette.fill.discrete = custom
    ))
  )
  expect_equal(overridden$data[[1]]$colour, custom(2)[as.integer(data$group)])
  expect_equal(overridden$data[[1]]$fill, custom(2)[as.integer(data$group)])

  shared <- ggplot2::ggplot_build(base + scale_discrete_coursekata())
  expect_identical(
    shared$plot$scales$get_scales("colour"), shared$plot$scales$get_scales("fill")
  )
  expect_equal(shared$data[[1]]$colour, expected)
  expect_equal(shared$data[[1]]$fill, expected)
})

test_that("default colour and fill scales train on their own category sets", {
  local_theme_lifecycle_state()
  coursekata_load_theme()
  data <- data.frame(
    x = 1:4, colour = c("a", "b", "a", "b"), fill = c("c", "d", "e", "c")
  )
  p <- ggplot2::ggplot(data, ggplot2::aes(x, x, colour = colour, fill = fill)) +
    ggplot2::geom_point(shape = 21)
  built <- ggplot2::ggplot_build(p)
  colour <- built$plot$scales$get_scales("colour")
  fill <- built$plot$scales$get_scales("fill")
  expect_false(identical(colour, fill))
  expect_identical(colour$get_limits(), c("a", "b"))
  expect_identical(fill$get_limits(), c("c", "d", "e"))
  palette <- coursekata_palette_provider()
  expect_equal(built$data[[1]]$colour, palette(2)[c(1, 2, 1, 2)])
  expect_equal(built$data[[1]]$fill, palette(3)[c(1, 2, 3, 1)])

  shared <- ggplot2::ggplot_build(p + scale_discrete_coursekata())
  expect_identical(shared$plot$scales$get_scales("colour")$get_limits(), letters[1:5])
  expect_equal(shared$data[[1]]$fill, palette(5)[c(3, 4, 5, 3)])
})
