rects_of <- function(p) {
  d <- ggplot2::ggplot_build(p)$data[[1]]
  d <- d[order(d$xmin, d$ymin), c("xmin", "xmax", "ymin", "ymax")]
  rownames(d) <- NULL
  d
}

test_that("integer data draws one column per integer, one square per observation", {
  p <- suppressMessages(gf_squareplot(~x, data = data.frame(x = c(1, 1, 2, 2, 2, 3))))
  r <- rects_of(p)
  expect_equal(nrow(r), 6)
  expect_equal(sort(unique(r$xmin)), c(1, 2, 3))
  expect_equal(sum(r$xmin == 2), 3)
  expect_equal(unique(r$xmax - r$xmin), 1)
})

test_that("every square is one unit tall, so the y axis counts observations", {
  p <- suppressMessages(gf_squareplot(~x, data = data.frame(x = rep(1, 7))))
  r <- rects_of(p)
  expect_true(all(r$ymax - r$ymin == 1))
  expect_equal(max(r$ymax), 7)
})

test_that("a bin over 75 observations keeps its squares instead of becoming a bar", {
  p <- suppressMessages(gf_squareplot(~x, data = data.frame(x = rep(5, 100))))
  expect_equal(length(p$layers), 1)
  expect_s3_class(p$layers[[1]]$geom, "GeomSquareplot")
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(nrow(built), 100)
  expect_equal(max(built$ymax), 100)
})

test_that("bars = 'solid' replaces the squares with one filled bar per bin", {
  d <- data.frame(x = c(1, 1, 2, 2, 2, 3))
  p <- suppressMessages(gf_squareplot(~x, data = d, bars = "solid", fill = "coral"))
  expect_equal(length(p$layers), 1)
  expect_false(inherits(p$layers[[1]]$geom, "GeomSquareplot"))
  bars <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(bars$ymin, rep(0, 3))
  expect_equal(bars$ymax, c(2, 3, 1))
  expect_equal(unique(bars$fill), "coral")
})

test_that("bars = 'outline' keeps the squares and frames them", {
  d <- data.frame(x = c(1, 1, 2, 2, 2, 3))
  p <- suppressMessages(gf_squareplot(~x, data = d, bars = "outline"))
  expect_equal(length(p$layers), 2)
  expect_s3_class(p$layers[[1]]$geom, "GeomSquareplot")
  expect_equal(nrow(ggplot2::ggplot_build(p)$data[[1]]), 6)
})

test_that("a factor's zero-count levels still appear on the axis", {
  df <- data.frame(x = factor(c(1, 1, 3, 3), levels = 1:5))
  p <- suppressMessages(gf_squareplot(~x, data = df, bars = "outline"))
  bars <- ggplot2::ggplot_build(p)$data[[2]]
  expect_equal(nrow(bars), 5)
  expect_equal(sum(bars$ymax == 0), 3)
  expect_equal(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$breaks, 1:5)
})

test_that("the squares layer honours fill, alpha, binwidth and origin", {
  d <- data.frame(x = c(1, 1, 2, 2, 2, 3))
  p <- suppressMessages(gf_squareplot(~x, data = d, fill = "coral", alpha = 0.4,
                                      binwidth = 2, origin = 0))
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(unique(built$fill), "coral")
  expect_equal(unique(built$alpha), 0.4)
  expect_equal(sort(unique(built$xmin)), c(0, 2))
})

test_that("gf_squareplot works from a bare numeric vector", {
  expect_s3_class(suppressMessages(gf_squareplot(Fingers$Thumb)), "gf_squareplot")
})

test_that("gf_squareplot errors on non-numeric input", {
  expect_error(suppressMessages(gf_squareplot(c("a", "b", "c"))), "must be numeric")
})

test_that("a variable that is defined nowhere is named, not resolved to something else", {
  expect_error(suppressMessages(gf_squareplot(~no_such_thing)), "no_such_thing")
})

test_that("data with nothing but missing values is refused rather than drawn empty", {
  expect_error(
    suppressMessages(gf_squareplot(~x, data = data.frame(x = c(NA_real_, NA_real_)))),
    "no non-missing"
  )
})

test_that("gf_squareplot print method suppresses warnings", {
  result <- suppressMessages(gf_squareplot(~Thumb, data = Fingers))
  expect_no_warning(print(result))
})

test_that("gf_squareplot snapshot", {
  skip_if_not_installed("vdiffr")
  int_data <- data.frame(x = c(1, 1, 2, 2, 2, 3, 3, 4, 5, 5))
  suppressMessages(gf_squareplot(~x, data = int_data)) %>%
    expect_doppelganger("gf_squareplot-basic-int")
})

test_that("a column that is not in the data is named, along with the columns that are", {
  msg <- tryCatch(
    suppressMessages(gf_squareplot(~Thmub, data = Fingers)),
    error = conditionMessage
  )
  expect_match(msg, "Thmub")
  expect_match(msg, "available:.*Thumb")
})

test_that("an expression rather than a bare variable is refused, not silently ignored", {
  expect_error(suppressMessages(gf_squareplot(~ log(Thumb), data = Fingers)), "log\\(Thumb\\)")
})

test_that("a factor with non-numeric levels says what it found", {
  msg <- tryCatch(
    suppressMessages(gf_squareplot(~Sex, data = Fingers)),
    error = conditionMessage
  )
  expect_match(msg, "Sex")
  expect_match(msg, "levels:.*female")
})

test_that("na.rm = FALSE is refused rather than crashing or drawing NA rectangles", {
  d <- data.frame(x = c(1, 2, 2, NA))
  expect_error(suppressMessages(gf_squareplot(~x, data = d, na.rm = FALSE)), "na.rm")
  expect_s3_class(suppressMessages(gf_squareplot(~x, data = d)), "gf_squareplot")
})

test_that("the na.rm message reads as prose, with no baked-in line breaks", {
  d <- data.frame(x = c(1, 2, NA))
  msg <- tryCatch(
    suppressMessages(gf_squareplot(~x, data = d, na.rm = FALSE)),
    error = conditionMessage
  )
  expect_false(any(grepl("\n\\s\\s+", strsplit(msg, "•")[[1]])))
})

test_that("factor levels that look numeric but have odd formatting still round-trip", {
  df <- data.frame(x = factor(c("01", " 2", "3.0"), levels = c("01", " 2", "3.0")))
  p <- suppressMessages(gf_squareplot(~x, data = df, bars = "outline"))
  bars <- ggplot2::ggplot_build(p)$data[[2]]
  expect_equal(sort(bars$xmin), c(1, 2, 3))
})

test_that("a two-sided formula is refused rather than silently plotting the RHS", {
  expect_error(suppressMessages(gf_squareplot(Thumb ~ Height, data = Fingers)), "one-sided")
})

test_that("a formula variable resolves from the calling function's environment", {
  from_bare <- function() {
    some_local_variable <- c(1, 2, 2, 3, 3, 3)
    gf_squareplot(some_local_variable)
  }
  from_formula <- function() {
    some_local_variable <- c(1, 2, 2, 3, 3, 3)
    gf_squareplot(~some_local_variable)
  }
  expect_s3_class(suppressMessages(from_bare()), "gf_squareplot")
  expect_s3_class(suppressMessages(from_formula()), "gf_squareplot")
})

draw_on_a_device <- function(p) {
  grDevices::pdf(NULL, width = 7, height = 4.5)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.draw(ggplot2::ggplotGrob(p))
}

test_that("a large distribution still draws one square per observation", {
  set.seed(24)
  p <- suppressMessages(gf_squareplot(~x, data = data.frame(x = rnorm(2000, 50, 10))))
  expect_equal(nrow(ggplot2::ggplot_build(p)$data[[1]]), 2000)
})

test_that("a large distribution draws its fitted border without complaint", {
  set.seed(24)
  p <- suppressMessages(gf_squareplot(~x, data = data.frame(x = rnorm(2000, 50, 10))))
  expect_no_warning(draw_on_a_device(p))
})

test_that("an explicit linewidth that hides observations warns, naming the sizes", {
  set.seed(24)
  p <- ggplot2::ggplot(data.frame(x = rnorm(2000, 50, 10)), ggplot2::aes(x = .data$x)) +
    ggplot2::layer(
      geom = GeomSquareplot, stat = StatSquareplot, position = "identity",
      params = list(linewidth = 2, fill = "coral", colour = "white")
    )
  w <- expect_warning(draw_on_a_device(p), class = "coursekata_squares_hidden")
  msg <- conditionMessage(w)
  expect_match(msg, "2000 observations")
  expect_match(msg, "each square is [0-9.]+ pt across")
  expect_match(msg, "border is [0-9.]+ pt wide")
})

test_that("the fitted stroke reaches the grob that is actually drawn", {
  set.seed(24)
  p <- suppressMessages(gf_squareplot(~x, data = data.frame(x = rnorm(2000, 50, 10))))
  grDevices::pdf(NULL, width = 7, height = 4.5)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(p)
  grid::grid.force()
  drawn <- grid::grid.get("geom_rect", grep = TRUE, global = TRUE)
  expect_s3_class(drawn, "rect")
  expect_equal(length(drawn$x), 2000)
  # an unfired makeContent leaves the untouched 1.42pt stroke, or no lwd at all
  expect_length(drawn$gp$lwd, 1)
  expect_lt(drawn$gp$lwd, 0.5 * ggplot2::.pt)
})
