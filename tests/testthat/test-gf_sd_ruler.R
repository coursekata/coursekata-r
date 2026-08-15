ruler_of <- function(p) {
  built <- ggplot2::ggplot_build(p)
  built$data[[layer_index(p, "sd_ruler")]]
}

test_that("gf_sd_ruler adds one tagged segment layer to a plot", {
  p <- gf_jitter(Thumb ~ Height, data = Fingers)
  result <- suppressMessages(gf_sd_ruler(p))

  expect_s3_class(result, "ggplot")
  expect_equal(length(result$layers), 2L)
  expect_equal(layer_index(result, "sd_ruler"), 2L)
  expect_equal(class(result$layers[[2]]$geom)[[1]], "GeomSegment")
})

test_that("gf_sd_ruler puts the ruler where the `where` argument says", {
  p <- gf_jitter(Thumb ~ Height, data = Fingers)
  at <- function(where) ruler_of(suppressMessages(gf_sd_ruler(p, where = where)))$x

  expect_equal(at("middle"), (min(Fingers$Height) + max(Fingers$Height)) / 2)
  expect_equal(at("mean"), mean(Fingers$Height))
  expect_equal(at("median"), stats::median(Fingers$Height))
  expect_length(unique(c(at("middle"), at("mean"), at("median"))), 3L)
})

test_that("the ruler runs from the mean up by one standard deviation", {
  p <- gf_jitter(Thumb ~ Height, data = Fingers)
  seg <- ruler_of(suppressMessages(gf_sd_ruler(p)))

  expect_equal(seg$y, mean(Fingers$Thumb))
  expect_equal(seg$yend, mean(Fingers$Thumb) + sd(Fingers$Thumb))
})

test_that("gf_sd_ruler measures the x variable on a histogram", {
  p <- suppressMessages(gf_histogram(~Thumb, data = Fingers, bins = 30))
  seg <- ruler_of(suppressMessages(gf_sd_ruler(p)))

  expect_equal(seg$x, mean(Fingers$Thumb))
  expect_equal(seg$xend, mean(Fingers$Thumb) + sd(Fingers$Thumb))
  expect_equal(seg$y, 0)
  expect_equal(seg$yend, 0)
})

test_that("a formula names the variables when the plot does not", {
  seg <- ruler_of(suppressMessages(gf_sd_ruler(Thumb ~ Height, data = Fingers)))
  expect_equal(seg$y, mean(Fingers$Thumb))
  expect_equal(seg$x, (min(Fingers$Height) + max(Fingers$Height)) / 2)

  flat <- ruler_of(suppressMessages(gf_sd_ruler(~Thumb, data = Fingers)))
  expect_equal(flat$x, mean(Fingers$Thumb))
  expect_equal(flat$yend, 0)
})

test_that("a formula reads the data before the calling environment", {
  Thumb <- "Height" # nolint: object_name_linter.
  p <- gf_jitter(Thumb ~ Height, data = Fingers)

  seg <- ruler_of(suppressMessages(gf_sd_ruler(p, Thumb ~ Height)))
  expect_equal(seg$y, mean(Fingers$Thumb))
})

test_that("gf_sd_ruler measures a variable the plot maps only on its layer", {
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(Height, Thumb), data = Fingers)
  seg <- ruler_of(suppressMessages(gf_sd_ruler(p)))
  expect_equal(seg$y, mean(Fingers$Thumb))
  expect_equal(seg$yend, mean(Fingers$Thumb) + sd(Fingers$Thumb))

  h <- ggplot2::ggplot(Fingers) + ggplot2::geom_histogram(ggplot2::aes(Thumb), bins = 30)
  flat <- ruler_of(suppressMessages(gf_sd_ruler(h)))
  expect_equal(flat$x, mean(Fingers$Thumb))
})

test_that("gf_sd_ruler recovers an axis the plot maps only on its first layer", {
  p <- ggplot2::ggplot(Fingers, ggplot2::aes(x = Height)) +
    ggplot2::geom_point(ggplot2::aes(y = Thumb))
  seg <- ruler_of(suppressMessages(gf_sd_ruler(p)))
  expect_equal(seg$y, mean(Fingers$Thumb))
  expect_equal(seg$yend, mean(Fingers$Thumb) + sd(Fingers$Thumb))
  expect_equal(seg$x, seg$xend)

  q <- ggplot2::ggplot(Fingers, ggplot2::aes(y = Thumb)) +
    ggplot2::geom_point(ggplot2::aes(x = Height))
  seg2 <- ruler_of(suppressMessages(gf_sd_ruler(q)))
  expect_equal(seg2$y, mean(Fingers$Thumb))
  expect_equal(seg2$x, (min(Fingers$Height) + max(Fingers$Height)) / 2)
})

test_that("a recovered formula still measures each facet's own subset", {
  p <- ggplot2::ggplot(Fingers, ggplot2::aes(x = Height)) +
    ggplot2::geom_point(ggplot2::aes(y = Thumb)) +
    ggplot2::facet_wrap(~Sex)
  seg <- ruler_of(suppressMessages(gf_sd_ruler(p)))

  expect_equal(nrow(seg), 2L)
  expect_equal(seg$y, as.numeric(tapply(Fingers$Thumb, Fingers$Sex, mean)))
})

test_that("a plot that names both axes is left to speak for itself", {
  result <- suppressMessages(gf_jitter(Thumb ~ Height, data = Fingers) %>% gf_sd_ruler())

  expect_length(result$layers[[2]]$mapping, 0L)
  expect_s3_class(result$layers[[2]]$data, "waiver")
})

test_that("a recovered ruler measures the data the layer draws", {
  scaled <- transform(Fingers, Thumb = Thumb * 100)
  p <- ggplot2::ggplot(scaled, ggplot2::aes(x = Height)) +
    ggplot2::geom_point(ggplot2::aes(y = Thumb), data = Fingers)
  seg <- ruler_of(suppressMessages(gf_sd_ruler(p)))

  expect_equal(seg$y, mean(Fingers$Thumb))
})

test_that("a plot with no x anywhere is still refused by the stat", {
  p <- ggplot2::ggplot(Fingers) + ggplot2::geom_point(ggplot2::aes(y = Thumb))

  expect_error(
    ggplot2::ggplot_build(suppressMessages(gf_sd_ruler(p))),
    "missing aesthetics"
  )
})

test_that("a plot with no layers of its own still gets a ruler", {
  seg <- ruler_of(suppressMessages(gf_sd_ruler(ggplot2::ggplot(Fingers, ggplot2::aes(x = Thumb)))))

  expect_equal(seg$x, mean(Fingers$Thumb))
})

test_that("gf_sd_ruler measures the values the plot draws", {
  # a computed mapping used to be refused outright
  p <- suppressMessages(gf_histogram(~ log(Thumb), data = Fingers, bins = 30))
  seg <- ruler_of(suppressMessages(gf_sd_ruler(p)))
  expect_equal(seg$x, mean(log(Fingers$Thumb)))
  expect_equal(seg$xend, mean(log(Fingers$Thumb)) + sd(log(Fingers$Thumb)))

  # a transformed axis is measured in the space it is drawn in
  logged <- gf_point(Thumb ~ Height, data = Fingers) + ggplot2::scale_y_log10()
  seg <- ruler_of(suppressMessages(gf_sd_ruler(logged)))
  expect_equal(seg$y, mean(log10(Fingers$Thumb)))
  expect_equal(seg$yend, mean(log10(Fingers$Thumb)) + sd(log10(Fingers$Thumb)))
})

test_that("gf_sd_ruler measures each facet's own subset", {
  p <- gf_jitter(Thumb ~ Height | Sex, data = Fingers)
  seg <- ruler_of(suppressMessages(gf_sd_ruler(p)))

  by_sex <- tapply(Fingers$Thumb, Fingers$Sex, mean)
  expect_equal(nrow(seg), 2L)
  expect_equal(seg$y, as.numeric(by_sex))
})

test_that("the ruler is placed where the points are actually drawn", {
  # placement used to re-derive categorical positions in order of appearance
  # while the axis orders them alphabetically, so the two disagreed
  d <- data.frame(g = c("zeta", "zeta", "zeta", "alpha"), v = c(1, 2, 3, 4))
  p <- gf_point(v ~ g, data = d)
  drawn <- ggplot2::ggplot_build(p)$data[[1]]$x

  expect_equal(ruler_of(suppressMessages(gf_sd_ruler(p, where = "mean")))$x, mean(drawn))
  expect_equal(
    ruler_of(suppressMessages(gf_sd_ruler(p, where = "median")))$x,
    stats::median(drawn)
  )
})

test_that("gf_sd_ruler rejects an outcome it cannot measure", {
  categorical <- suppressMessages(gf_bar(~Sex, data = Fingers))
  expect_error(
    ggplot2::ggplot_build(suppressMessages(gf_sd_ruler(categorical))),
    "quantitative outcome"
  )

  p <- gf_jitter(Thumb ~ Height, data = Fingers)
  expect_error(
    ggplot2::ggplot_build(suppressMessages(gf_sd_ruler(p, Nope ~ Height))),
    "Nope"
  )
  expect_error(suppressMessages(gf_sd_ruler(p, where = "moddle")), "where")
})

test_that("a name the layer cannot use is discarded, the way every gf_ layer discards one", {
  # `y` and `x` used to be arguments here. They are now written in the formula,
  # so they are names the layer does not recognise -- and ggformula discards one
  # of those without a word. This package does not add a rule of its own
  p <- gf_jitter(Thumb ~ Height, data = Fingers)

  expect_no_error(suppressMessages(gf_sd_ruler(p, y = "Thumb")))
  expect_no_error(suppressMessages(gf_point(Thumb ~ Height, data = Fingers, nonesuch = 1)))
})

test_that("`size` still works, and says what to write instead", {
  p <- suppressMessages(gf_histogram(~Thumb, data = Fingers, bins = 30))

  expect_warning(suppressMessages(gf_sd_ruler(p, size = 2)), "linewidth")
  expect_equal(
    suppressWarnings(ruler_of(suppressMessages(gf_sd_ruler(p, size = 2))))$linewidth,
    2
  )
  expect_equal(ruler_of(suppressMessages(gf_sd_ruler(p, linewidth = 2)))$linewidth, 2)
})

test_that("an aesthetic the ruler cannot honour is refused, not swallowed", {
  p <- gf_jitter(Thumb ~ Height, data = Fingers)
  expect_error(suppressMessages(gf_sd_ruler(p, color = ~Sex)), "can't be mapped")
  expect_error(suppressMessages(gf_sd_ruler(p, color = ~Sex)), "y ~ x | group")
})

test_that("the ruler is red and 0.8 wide unless told otherwise", {
  p <- gf_jitter(Thumb ~ Height, data = Fingers)
  expect_equal(ruler_of(suppressMessages(gf_sd_ruler(p)))$colour, "red")
  expect_equal(ruler_of(suppressMessages(gf_sd_ruler(p)))$linewidth, 0.8)
  expect_equal(ruler_of(suppressMessages(gf_sd_ruler(p, color = "blue")))$colour, "blue")
  expect_equal(ruler_of(suppressMessages(gf_sd_ruler(p, colour = "blue")))$colour, "blue")
})

test_that("gf_sd_ruler composes like every other gf_ function", {
  labelled <- suppressMessages(
    gf_sd_ruler(Thumb ~ Height, data = Fingers, title = "TT", xlab = "XX", ylab = "YY")
  )
  expect_equal(labelled$labels$title, "TT")
  expect_equal(labelled$labels$x, "XX")
  expect_equal(labelled$labels$y, "YY")

  piped <- ruler_of(suppressMessages(Fingers %>% gf_sd_ruler(~Thumb)))
  expect_equal(piped$x, mean(Fingers$Thumb))

  faceted <- suppressMessages(gf_sd_ruler(Thumb ~ Height | Sex, data = Fingers))
  expect_equal(nrow(ruler_of(faceted)), 2L)
})

test_that("gf_sd_ruler snapshot", {
  skip_if_not_installed("vdiffr")
  p <- gf_jitter(Thumb ~ Height, data = Fingers, seed = 42)

  suppressMessages(gf_sd_ruler(p, color = "red")) %>%
    expect_doppelganger("gf_sd_ruler-basic")
})

test_that("the ruler is found by tag, not by position, when layers follow it", {
  p <- gf_jitter(Thumb ~ Height, data = Fingers)
  result <- suppressMessages(gf_sd_ruler(p)) + ggplot2::geom_rug()

  seg <- ruler_of(result)
  expect_equal(seg$y, mean(Fingers$Thumb))
  expect_equal(seg$yend, mean(Fingers$Thumb) + sd(Fingers$Thumb))
})
