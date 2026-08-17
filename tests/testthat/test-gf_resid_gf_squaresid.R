test_that("gf_resid draws one residual per observation, from the fit to the point", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers) %>% gf_resid(model)
  i <- layer_index(p, "resid")
  drawn <- ggplot2::ggplot_build(p)$data[[i]]

  expect_true(inherits(p$layers[[i]]$geom, "GeomResid"))
  expect_equal(nrow(drawn), nrow(Fingers))
  expect_equal(drawn$yend, unname(stats::predict(model)))
  expect_equal(drawn$y, Fingers$Thumb)
})

test_that("gf_square_resid draws one square per observation", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- suppressMessages(gf_point(Thumb ~ Height, data = Fingers) %>% gf_square_resid(model))
  i <- layer_index(p, "square_resid")

  expect_true(inherits(p$layers[[i]]$geom, "GeomSquareResid"))
  expect_equal(nrow(ggplot2::ggplot_build(p)$data[[i]]), nrow(Fingers))
  expect_length(unique(ggplot2::layer_grob(p, i)[[1]]$id), nrow(Fingers))
})

test_that("gf_squaresid is a supported alias of gf_square_resid", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- suppressMessages(gf_point(Thumb ~ Height, data = Fingers) %>% gf_squaresid(model))

  expect_true(inherits(p$layers[[layer_index(p, "square_resid")]]$geom, "GeomSquareResid"))
})

test_that("a square is drawn only in the panel its observation belongs to", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- suppressMessages(
    gf_point(Thumb ~ Height, data = Fingers) %>%
      gf_facet_wrap(~RaceEthnic) %>%
      gf_square_resid(model)
  )
  built <- ggplot2::ggplot_build(p)

  expect_equal(
    table(built$data[[layer_index(p, "square_resid")]]$PANEL),
    table(built$data[[1]]$PANEL)
  )
  expect_gt(length(unique(table(built$data[[1]]$PANEL))), 1)
})

test_that("a model fit on fewer rows than the data holds still draws every point it can", {
  # lm() drops Fingers' 29 rows with a missing SSLast; the plot draws the other 128
  model <- lm(SSLast ~ Height, data = Fingers)
  p <- gf_point(SSLast ~ Height, data = Fingers) %>% gf_resid(model)

  segments <- suppressWarnings(ggplot2::layer_grob(p, layer_index(p, "resid"))[[1]])
  points <- suppressWarnings(ggplot2::layer_grob(p, 1)[[1]])
  expect_length(segments$x0, 128)
  expect_length(points$x, 128)
})

test_that("a model whose predictors are not in the plot's data names them", {
  model <- lm(Thumb ~ Weight, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers[c("Thumb", "Height")])

  expect_error(gf_resid(p, model), "Weight")
  expect_error(suppressMessages(gf_square_resid(p, model)), "does not have")
})

test_that("a plot with no outcome on an axis is refused, and the refusal names what is absent", {
  model <- lm(Thumb ~ Height, data = Fingers)
  no_y <- gf_histogram(~Thumb, data = Fingers)
  expect_error(gf_resid(no_y, model), "x and a y")
  expect_error(gf_resid(no_y, model), "missing: y")

  nothing <- ggplot2::ggplot(Fingers) + ggplot2::geom_point()
  expect_error(gf_resid(nothing, model), "the plot maps: nothing")
  expect_error(gf_resid(nothing, model), "missing: x, y")
  expect_error(suppressMessages(gf_square_resid(nothing, model)), "missing: x, y")

  # aes(x = NULL) keeps the name and drops the mapping, so a name is not enough
  unmapped <- ggplot2::ggplot(Fingers, ggplot2::aes(x = NULL, y = Thumb)) +
    ggplot2::geom_point()
  expect_error(gf_resid(unmapped, model), "missing: x")
})

test_that("gf_resid reads the axes a plot maps on its first layer", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- ggplot2::ggplot(Fingers) + ggplot2::geom_point(ggplot2::aes(x = Height, y = Thumb))
  r <- gf_resid(p, model)
  drawn <- ggplot2::ggplot_build(r)$data[[layer_index(r, "resid")]]

  expect_equal(nrow(drawn), nrow(Fingers))
  expect_equal(drawn$x, Fingers$Height)
  expect_equal(drawn$y, Fingers$Thumb)
  expect_equal(drawn$yend, unname(stats::predict(model)))
})

test_that("gf_square_resid reads the axes a plot maps on its first layer", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- ggplot2::ggplot(Fingers) + ggplot2::geom_point(ggplot2::aes(x = Height, y = Thumb))
  r <- suppressMessages(gf_square_resid(p, model))
  i <- layer_index(r, "square_resid")
  drawn <- ggplot2::ggplot_build(r)$data[[i]]

  expect_equal(nrow(drawn), nrow(Fingers))
  expect_equal(drawn$yend, unname(stats::predict(model)))
  expect_length(unique(ggplot2::layer_grob(r, i)[[1]]$id), nrow(Fingers))
})

test_that("a residual reads the data the first layer holds when the plot holds none", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = Fingers, mapping = ggplot2::aes(x = Height, y = Thumb))
  r <- gf_resid(p, model)
  drawn <- ggplot2::ggplot_build(r)$data[[layer_index(r, "resid")]]

  expect_equal(nrow(drawn), nrow(Fingers))
  expect_equal(drawn$yend, unname(stats::predict(model)))
})

test_that("a square takes the geom's own colour rather than the one the plot maps", {
  model <- lm(Thumb ~ Height, data = Fingers)
  r <- suppressMessages(
    gf_point(Thumb ~ Height, color = ~Sex, data = Fingers) %>% gf_square_resid(model)
  )
  drawn <- ggplot2::ggplot_build(r)$data[[layer_index(r, "square_resid")]]

  # GeomPolygon's own colour is NA; the points are drawn #6929c4 and #009d9a
  expect_true(all(is.na(drawn$colour)))
})

test_that("gf_resid segments are anchored to the jittered points", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  p <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>% gf_resid(model)
  built <- ggplot2::ggplot_build(p)
  points <- built$data[[1]]
  segments <- built$data[[layer_index(p, "resid")]]

  # the observed end follows the jitter; the fitted end stays on the model
  expect_equal(segments$x, points$x)
  expect_equal(segments$y, points$y)
  expect_equal(segments$yend, unname(stats::predict(model)))
})

test_that("the points keep their jitter however many layers the plot already has", {
  # The residual and its point agree because both layers declare the same
  # jittered position, not because anything replays anything: two layers sharing
  # a seed land on identical offsets. The design this replaced reached back into
  # the plot and rewrote the points layer's position, which under ggplot2 4 left
  # that position without a width -- a jitter with no width offsets nothing, so
  # every point snapped to its group's centre. It only bit once the plot carried
  # a second layer, which is every documented pipeline, and no test had one.
  model <- lm(Thumb ~ Sex, data = Fingers)
  layered <- suppressMessages(
    gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>%
      gf_model(model) %>%
      gf_resid(model)
  )
  built <- ggplot2::ggplot_build(layered)
  points <- built$data[[1]]
  segments <- built$data[[layer_index(layered, "resid")]]

  expect_gt(length(unique(points$x)), 100)
  expect_equal(segments$x, points$x)
  expect_equal(segments$y, points$y)
  # and the fitted end stays on the model rather than being jittered with them
  expect_equal(segments$yend, unname(stats::predict(model)))
})

test_that("two overlays on one plot land on the same dots", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  p <- suppressMessages(
    gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>%
      gf_resid(model) %>%
      gf_square_resid(model)
  )
  built <- ggplot2::ggplot_build(p)

  expect_equal(built$data[[layer_index(p, "resid")]]$x, built$data[[1]]$x)
  expect_equal(built$data[[layer_index(p, "square_resid")]]$x, built$data[[1]]$x)
})

test_that("a jittered segment stays on its point when the model drops rows for missingness", {
  # SSLast has 29 missing values; Sex does not, so predict() never returns NA -- the
  # mismatch is between how many rows the point layer's jitter sees and how many the
  # residual's replayed jitter would see if the stat removed the incomplete ones first
  model <- lm(SSLast ~ Sex, data = Fingers)
  p <- gf_jitter(SSLast ~ Sex, data = Fingers, width = .1) %>% gf_resid(model)
  built <- suppressWarnings(ggplot2::ggplot_build(p))
  points <- built$data[[1]]
  segments <- built$data[[layer_index(p, "resid")]]

  expect_equal(nrow(segments), nrow(points))
  expect_equal(segments$x, points$x)
  expect_equal(segments$y, points$y)
})

test_that("a squared residual's four corners share the point's x, not four jitters of it", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  p <- suppressMessages(
    gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>% gf_square_resid(model)
  )
  points <- as.numeric(ggplot2::layer_grob(p, 1)[[1]]$x)
  corners <- as.numeric(ggplot2::layer_grob(p, layer_index(p, "square_resid"))[[1]]$x)

  expect_length(unique(round(corners[1:4], 10)), 2)
  expect_equal(corners[seq(1, length(corners), by = 4)], points)
  expect_equal(corners[seq(4, length(corners), by = 4)], points)
})

test_that("squares are drawn square on the page, in the panel they end up in", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  p <- suppressMessages(
    gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>%
      gf_square_resid(model, aspect = 1)
  )
  grob <- ggplot2::layer_grob(p, layer_index(p, "square_resid"))[[1]]
  side <- function(axis) {
    as.numeric(tapply(as.numeric(grob[[axis]]), grob$id, function(z) diff(range(z))))
  }

  expect_equal(side("x"), side("y"))
})

test_that("jittered positions are stable across repeated builds", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  plot <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>%
    gf_resid(model)

  build_1 <- ggplot2::ggplot_build(plot)$data[[1]]
  build_2 <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_identical(build_1$x, build_2$x)
  expect_identical(build_1$y, build_2$y)
})

test_that("gf_resid segments inherit the plot's mapped aesthetics", {
  model <- lm(Thumb ~ Height, data = Fingers)

  colored <- gf_resid(gf_point(Thumb ~ Height, color = ~Sex, data = Fingers), model)
  color_segments <- ggplot2::ggplot_build(colored)$data[[layer_index(colored, "resid")]]
  expect_gt(length(unique(color_segments$colour)), 1)

  faded <- gf_resid(gf_point(Thumb ~ Height, alpha = ~Height, data = Fingers), model)
  alpha_segments <- ggplot2::ggplot_build(faded)$data[[layer_index(faded, "resid")]]
  expect_gt(length(unique(alpha_segments$alpha)), 1)
})

test_that("gf_resid does not reset the user's RNG stream", {
  withr::local_preserve_seed()
  model <- lm(Thumb ~ Sex, data = Fingers)

  # the bracket this replaced reseeded with sample(1:100, 1)
  reachable <- lapply(1:100, function(seed) {
    set.seed(seed)
    .Random.seed
  })

  set.seed(20250711)
  invisible(gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>% gf_resid(model))

  expect_false(any(vapply(reachable, identical, logical(1), .Random.seed)))
})

test_that("gf_resid seeds a copy of the jitter position, not the shared object", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  base <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1)

  # hold a reference to the layer's position object before freezing. geom_jitter()
  # and position = "jitter" share ggplot2's namespace-level PositionJitter, so
  # writing a seed onto this object in place would leak it into unrelated plots.
  original_pos <- base$layers[[1]]$position
  expect_false(is.finite(original_pos$seed))

  frozen <- gf_resid(base, model)

  # the frozen plot carries a seed, but on a *new* object -- the one we captured
  # is left untouched, proving the seed was copied on rather than mutated in
  expect_true(is.finite(frozen$layers[[1]]$position$seed))
  expect_false(identical(frozen$layers[[1]]$position, original_pos))
  expect_false(is.finite(original_pos$seed))
})

# Orientation ------------------------------------------------------------------------------------

test_that("a residual for a model of the plot's x variable is measured on x", {
  model <- lm(Height ~ Thumb, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers) %>% gf_resid(model)
  drawn <- ggplot2::ggplot_build(p)$data[[layer_index(p, "resid")]]

  expect_equal(drawn$xend, unname(stats::predict(model)))
  expect_null(drawn$yend)
  expect_equal(drawn$x, Fingers$Height)
  expect_equal(drawn$y, Fingers$Thumb)
})

test_that("an x-outcome residual is drawn across x and holds its observation's y", {
  base <- gf_point(Thumb ~ Height, data = Fingers)

  flipped_model <- lm(Height ~ Thumb, data = Fingers)
  flipped <- base %>% gf_resid(flipped_model)
  flipped_grob <- ggplot2::layer_grob(flipped, layer_index(flipped, "resid"))[[1]]
  x0 <- as.numeric(flipped_grob$x0)
  x1 <- as.numeric(flipped_grob$x1)
  y0 <- as.numeric(flipped_grob$y0)
  y1 <- as.numeric(flipped_grob$y1)

  expect_equal(y0, y1)
  expect_false(isTRUE(all.equal(x0, x1)))

  upright_model <- lm(Thumb ~ Height, data = Fingers)
  upright <- base %>% gf_resid(upright_model)
  upright_grob <- ggplot2::layer_grob(upright, layer_index(upright, "resid"))[[1]]
  ux0 <- as.numeric(upright_grob$x0)
  ux1 <- as.numeric(upright_grob$x1)
  uy0 <- as.numeric(upright_grob$y0)
  uy1 <- as.numeric(upright_grob$y1)

  expect_equal(ux0, ux1)
  expect_false(isTRUE(all.equal(uy0, uy1)))
})

test_that("an x-outcome residual ends on the model the plot draws", {
  model <- lm(Height ~ Thumb, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers) %>% gf_model(model) %>% gf_resid(model)
  built <- ggplot2::ggplot_build(p)
  line <- built$data[[layer_index(p, "model")]]
  segments <- built$data[[layer_index(p, "resid")]]

  on_the_line <- stats::approxfun(line$y, line$x)(segments$y)
  expect_equal(segments$xend, on_the_line, tolerance = 1e-10)
})

test_that("an x-outcome squared residual squares the residual it measures", {
  model <- lm(Height ~ Thumb, data = Fingers)
  p <- suppressMessages(
    gf_point(Thumb ~ Height, data = Fingers) %>% gf_square_resid(model, aspect = 1)
  )
  grob <- ggplot2::layer_grob(p, layer_index(p, "square_resid"))[[1]]
  panel <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  side <- function(axis) {
    as.numeric(tapply(as.numeric(grob[[axis]]), grob$id, function(z) diff(range(z))))
  }

  fitted <- unname(stats::predict(model))
  expect_equal(side("x"), abs(Fingers$Height - fitted) / diff(panel$x.range))
  expect_equal(side("x"), side("y"))
})

test_that("the prediction is named on the outcome's axis and never on x or y", {
  upright <- lm(Thumb ~ Height, data = Fingers)
  flipped <- lm(Height ~ Thumb, data = Fingers)
  cases <- list(
    list(gf_resid, upright, "yend"),
    list(gf_resid, flipped, "xend"),
    list(gf_square_resid, upright, "yend"),
    list(gf_square_resid, flipped, "xend")
  )

  for (case in cases) {
    fn <- case[[1]]
    model <- case[[2]]
    end <- case[[3]]
    p <- suppressMessages(fn(gf_point(Thumb ~ Height, data = Fingers), model))
    mapping <- p$layers[[length(p$layers)]]$mapping

    expect_equal(intersect(names(mapping), c("xend", "yend")), end)
    positional <- mapping[intersect(names(mapping), c("x", "y"))]
    labels <- vapply(positional, rlang::as_label, character(1))
    expect_false(any(grepl(".fitted", labels, fixed = TRUE)))
  }
})

test_that("a residual with no prediction to draw to is dropped, and named as itself", {
  model <- lm(Thumb ~ SSLast, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers) %>% gf_resid(model)

  expect_warning(
    grob <- ggplot2::layer_grob(p, layer_index(p, "resid"))[[1]],
    "geom_resid"
  )
  expect_length(grob$x0, 128)
})

test_that("a model whose outcome the plot does not draw is refused, not measured", {
  # the segment would run from an observation of one variable to a prediction of
  # another; gf_model() refuses the same pairing, so the residual family does too
  p <- gf_point(Thumb ~ Height, data = Fingers)
  wrong <- lm(Weight ~ Height, data = Fingers)
  for (draw in list(gf_resid, gf_square_resid, gf_squaresid)) {
    expect_error(draw(p, wrong), "axis carrying the model's outcome")
  }
  expect_error(gf_resid(p, wrong), "the model predicts: Weight")
  expect_error(gf_resid(p, wrong), "y = Thumb")

  # the model the plot was built for still draws, on either orientation
  expect_no_error(gf_resid(p, lm(Thumb ~ Height, data = Fingers)))
  expect_no_error(gf_resid(gf_point(Height ~ Thumb, data = Fingers),
                           lm(Thumb ~ Height, data = Fingers)))
  # and a function of x has no outcome to be wrong about
  expect_no_error(gf_resid_fun(p, function(x) 60))
})


# gf_resid() is generated by ggformula::layer_factory(), which reads its extras
# out of `match.call()` and builds the layer through a signature nobody wrote by
# hand. These are the contract: what the generated function has to keep doing
# that the bespoke one did, and the things the generated one could lose in
# silence.

test_that("a model given positionally lands where a model given by name does", {
  # `layer_factory()` binds the second positional argument to `gformula`, and
  # `gf_resid(p, m)` is the spelling in every test, example and lesson
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  positional <- gf_resid(p, model)
  named <- gf_resid(p, model = model)

  expect_equal(layer_index(positional, "resid"), 2L)
  expect_equal(layer_index(named, "resid"), 2L)
  expect_equal(
    ggplot2::ggplot_build(positional)$data[[2]],
    ggplot2::ggplot_build(named)$data[[2]]
  )
  expect_equal(ggplot2::ggplot_build(positional)$data[[2]]$yend, unname(predict(model)))
})

test_that("the model names what to draw, so it never reaches the layer as a parameter", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  expect_no_warning(drawn <- gf_resid(p, model = model))
  layer <- drawn$layers[[layer_index(drawn, "resid")]]
  expect_false("model" %in% names(layer$aes_params))
  expect_false("model" %in% names(layer$geom_params))
  expect_false("model" %in% names(layer$stat_params))
})

test_that("a bare call prints the layer's own help instead of drawing", {
  # `pre` runs ahead of ggformula's help gate, so everything it does has to
  # survive a call with no plot and no model
  expect_message(bare <- gf_resid(), "does not require a formula")
  expect_null(bare)

  p <- gf_point(Thumb ~ Height, data = Fingers)
  expect_message(asked <- gf_resid(p, show.help = TRUE), "key attributes")
  expect_null(asked)
})

test_that("the labelling arguments every ggformula layer takes reach the plot", {
  p <- gf_point(Thumb ~ Height, data = Fingers) %>%
    gf_resid(lm(Thumb ~ Height, data = Fingers), title = "TT", xlab = "XX", ylab = "YY")

  expect_equal(p$labels$title, "TT")
  expect_equal(p$labels$x, "XX")
  expect_equal(p$labels$y, "YY")
})

test_that("an unusable name is answered the way the family the caller wrote answers it", {
  # the two families disagree and each is right in its own idiom: a `gf_` function
  # discards an unusable name in silence, a plot assembled with `layer()` warns.
  # `gf_resid()` follows whichever one the caller is writing in.
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  expect_no_warning(gf_resid(p, model, nonesuch = 1))
  expect_no_warning(gf_point(Thumb ~ Height, data = Fingers, nonesuch = 1))
  expect_no_warning(gf_resid(p, model, color = "firebrick", alpha = .5))

  fitted <- transform(Fingers, .fitted = stats::predict(model, Fingers))
  expect_warning(
    ggplot2::ggplot_build(
      p + ggplot2::layer(
        geom = GeomResid, stat = StatResid, position = "identity",
        data = fitted, mapping = ggplot2::aes(yend = .data$.fitted),
        params = list(nonesuch = 1)
      )
    ),
    "unknown parameters"
  )
})

test_that("the plot is the first argument, and a name for it is not one", {
  # `plot` was the released spelling of the first argument. It is now `object`,
  # the name every generated ggformula layer uses, so `plot =` is an unknown
  # name like any other and ggformula discards it -- which leaves no plot, and
  # the refusal a caller sees is the one for that
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  expect_error(gf_resid(plot = p, model = model), "layered on top of a plot")
  expect_no_error(gf_resid(object = p, model = model))
})

test_that("the residual draws with its own geom, stat and position whatever is asked for", {
  # the generated signature carries all three, and a segment geom draws the
  # ends where they arrive rather than transposing them, which moves the picture
  # without moving the built data
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  asked <- gf_resid(p, model, geom = "segment", stat = "identity", position = "dodge")
  layer <- asked$layers[[layer_index(asked, "resid")]]

  expect_s3_class(layer$geom, "GeomResid")
  expect_s3_class(layer$stat, "StatResid")
  # an unjittered plot needs no offset, so the layer declares identity;
  # what matters is that the caller's `position =` did not reach it
  expect_s3_class(layer$position, "PositionIdentity")
})

test_that("residual segments keep their own default linewidth, and take an override", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  plain <- gf_resid(p, model)
  wider <- gf_resid(p, model, linewidth = 2)

  expect_equal(plain$layers[[layer_index(plain, "resid")]]$aes_params$linewidth, 0.2)
  expect_equal(wider$layers[[layer_index(wider, "resid")]]$aes_params$linewidth, 2)
})

test_that("a stray positional argument is refused by ggformula, not swallowed", {
  # the released signature was `gf_resid(plot, model, linewidth = 0.2, ...)`, so
  # a third positional argument set the line width; the generated signature's
  # third formal is `data`. `pre` fills `data` only when the caller left it
  # alone, so an argument that lands there reaches ggformula and gets the same
  # refusal `gf_point(y ~ x, d, 0.5)` gets -- this package adds no rule of its own
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  expect_error(gf_resid(p, model, 2), "must be a <data.frame>")
  expect_error(gf_point(Thumb ~ Height, Fingers, 2), "must be a <data.frame>")
})

test_that("an axis mapped against the frame the plot was built in still measures", {
  # ggformula rebinds every mapping quosure to the calling frame, and the x and
  # y quosures here came from the plot, where an expression may name something
  # that only the plot's own frame has
  built_elsewhere <- function() {
    shift <- 10
    gf_point(Thumb ~ I(Height + shift), data = Fingers)
  }
  p <- gf_resid(built_elsewhere(), lm(Thumb ~ Height, data = Fingers))

  expect_equal(ggplot2::ggplot_build(p)$data[[2]]$x, Fingers$Height + 10)
})

test_that("gf_resid() needs a plot to measure against", {
  expect_error(
    gf_resid(Fingers, lm(Thumb ~ Height, data = Fingers)),
    "layered on top of a plot"
  )
})

test_that("gf_resid() says so when it is not told which model to measure", {
  expect_error(
    gf_resid(gf_point(Thumb ~ Height, data = Fingers)),
    "which model to measure"
  )
})

# gf_square_resid() and gf_squaresid() are generated by ggformula::layer_factory()
# too. These are their half of the contract: what the generated pair have to keep
# doing that the bespoke pair did, what the released signature could lose in
# silence, and the assertion that the alias is the same call with one string
# changed.

test_that("a model given positionally to a square lands where a named one does", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  positional <- suppressMessages(gf_square_resid(p, model))
  named <- suppressMessages(gf_square_resid(p, model = model))
  aliased <- suppressMessages(gf_squaresid(p, model))

  expect_equal(layer_index(positional, "square_resid"), 2L)
  expect_equal(layer_index(aliased, "square_resid"), 2L)
  expect_equal(
    ggplot2::ggplot_build(positional)$data[[2]],
    ggplot2::ggplot_build(named)$data[[2]]
  )
  expect_equal(ggplot2::ggplot_build(positional)$data[[2]]$yend, unname(predict(model)))
})

test_that("a bare call prints the help of the name that was written", {
  expect_message(bare <- gf_square_resid(), "gf_square_resid\\(\\) does not require a formula")
  expect_null(bare)
  expect_message(aliased <- gf_squaresid(), "gf_squaresid\\(\\) does not require a formula")
  expect_null(aliased)

  p <- gf_point(Thumb ~ Height, data = Fingers)
  expect_message(asked <- gf_square_resid(p, show.help = TRUE), "key attributes")
  expect_null(asked)
})

test_that("the labelling arguments reach the plot a square is drawn on", {
  p <- suppressMessages(
    gf_point(Thumb ~ Height, data = Fingers) %>%
      gf_square_resid(lm(Thumb ~ Height, data = Fingers),
                      title = "TT", xlab = "XX", ylab = "YY")
  )

  expect_equal(p$labels$title, "TT")
  expect_equal(p$labels$x, "XX")
  expect_equal(p$labels$y, "YY")
})

test_that("each square function refuses under its own name", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  # `plot =` is an unknown name now, so ggformula discards it and the refusal is
  # the one for a call with no plot -- under each function's own name
  expect_error(gf_square_resid(plot = p, model = model), "`gf_square_resid\\(\\)` needs to be layered")
  expect_error(gf_squaresid(plot = p, model = model), "`gf_squaresid\\(\\)` needs to be layered")

  expect_error(gf_square_resid(Fingers, model), "`gf_square_resid\\(\\)` needs to be layered")
  expect_error(gf_squaresid(Fingers, model), "`gf_squaresid\\(\\)` needs to be layered")
  expect_error(gf_square_resid(p), "`gf_square_resid\\(\\)` needs to be told which model")
  expect_error(gf_squaresid(p), "`gf_squaresid\\(\\)` needs to be told which model")
})

test_that("a stray positional argument gets ggformula's own refusal, not one of ours", {
  # the released signature was `gf_square_resid(plot, model, aspect = 4/6,
  # alpha = 0.1, ...)`, and both positions really worked until this conversion.
  # The generated third formal is `data`, and `pre` fills it only when the
  # caller left it alone, so a stray third argument reaches ggformula and is
  # refused exactly as it is for any other gf_ function. The fourth lands
  # unnamed in `...`, where an unusable extra is discarded in silence -- also
  # exactly as it is for any other gf_ function. This package adds no rule of
  # its own in either direction.
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  expect_error(gf_square_resid(p, model, 0.5), "must be a <data.frame>")
  expect_error(gf_squaresid(p, model, 0.5), "must be a <data.frame>")
  expect_error(gf_point(Thumb ~ Height, Fingers, 0.5), "must be a <data.frame>")

  # an unusable extra is silent, the same as a misspelled parameter
  expect_no_error(suppressMessages(gf_square_resid(p, model, , 0.3)))
  expect_no_error(suppressMessages(gf_square_resid(p, model, nonesuch = 1)))
})

test_that("a squared residual draws with its own geom, stat and position whatever is asked for", {
  # at HEAD this call produced GeomPolygon, a live ggproto in `inherit.aes`, and
  # two warnings, because `geom`/`stat`/`position` shifted the bespoke helper's
  # positional arguments along
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  expect_no_warning(
    asked <- suppressMessages(
      gf_square_resid(p, model, geom = "polygon", stat = "identity", position = "dodge")
    )
  )
  layer <- asked$layers[[layer_index(asked, "square_resid")]]

  expect_s3_class(layer$geom, "GeomSquareResid")
  expect_s3_class(layer$stat, "StatResid")
  # an unjittered plot needs no offset, so the layer declares identity;
  # what matters is that the caller's `position =` did not reach it
  expect_s3_class(layer$position, "PositionIdentity")
  expect_false(isTRUE(layer$inherit.aes))
})

test_that("aspect sizes the square as a geom parameter and alpha shades it as an aesthetic", {
  # they travel by different routes and both have to arrive: `aspect` is a
  # `draw_panel()` formal, so it is a geom parameter; `alpha` is an aesthetic,
  # so it is an aes parameter and never a geom one
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)
  side <- function(plot) {
    grob <- ggplot2::layer_grob(plot, layer_index(plot, "square_resid"))[[1]]
    mean(tapply(as.numeric(grob$x), grob$id, function(z) diff(range(z))))
  }

  plain <- suppressMessages(gf_square_resid(p, model))
  square <- suppressMessages(gf_square_resid(p, model, aspect = 1))
  faded <- suppressMessages(gf_square_resid(p, model, alpha = 0.7))

  expect_equal(plain$layers[[2]]$geom_params$aspect, 4 / 6)
  expect_equal(square$layers[[2]]$geom_params$aspect, 1)
  expect_equal(side(plain), side(square) * 4 / 6)

  expect_equal(plain$layers[[2]]$aes_params$alpha, 0.1)
  expect_equal(faded$layers[[2]]$aes_params$alpha, 0.7)
  expect_null(faded$layers[[2]]$geom_params$alpha)
  expect_equal(side(faded), side(plain))
})

test_that("a square is drawn in its own colors, where a residual segment inherits the plot's", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, color = ~Sex, data = Fingers)
  drawn <- function(plot, tag) ggplot2::ggplot_build(plot)$data[[layer_index(plot, tag)]]

  squares <- suppressMessages(gf_square_resid(p, model))
  expect_true(all(is.na(drawn(squares, "square_resid")$colour)))
  expect_length(unique(drawn(squares, "square_resid")$fill), 1)
  expect_gt(length(unique(drawn(gf_resid(p, model), "resid")$colour)), 1)

  # the caller may still ask for the outline
  outlined <- suppressMessages(gf_square_resid(p, model, inherit = TRUE))
  expect_gt(length(unique(drawn(outlined, "square_resid")$colour)), 1)
})

test_that("the experimental signal fires on a real call and not on a bare help call", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)
  signalled <- character()

  testthat::local_mocked_bindings(
    signal_stage = function(stage, what, with = NULL, env = NULL) {
      signalled <<- c(signalled, paste0(stage, ":", what))
      invisible()
    },
    .package = "lifecycle"
  )

  suppressMessages(gf_square_resid(p, model))
  suppressMessages(p %>% gf_squaresid(model))
  expect_equal(signalled, c("experimental:gf_square_resid()", "experimental:gf_squaresid()"))

  # asking a function what it takes is not using it
  signalled <- character()
  suppressMessages(gf_square_resid())
  suppressMessages(gf_squaresid())
  suppressMessages(gf_square_resid(p, model, show.help = TRUE))
  expect_equal(signalled, character())
})

test_that("the alias is generated from the same factory call, with only its name changed", {
  # `gf_squaresid()` is its own `layer_factory()` call so that its refusals, its
  # help and its signal name the function the caller wrote. This is what pays
  # for the second call: every argument the factory stored on the two closures,
  # `pre` included, has to match after a mechanical rename
  square <- environment(gf_square_resid)
  alias <- environment(gf_squaresid)
  rename <- function(x) {
    gsub("gf_squaresid", "gf_square_resid", paste(deparse(x), collapse = "\n"), fixed = TRUE)
  }

  expect_setequal(ls(alias), ls(square))
  # `res` is the generated closure itself, and it is compared by its own parts
  for (stored in setdiff(ls(square), "res")) {
    expect_identical(rename(get(stored, alias)), rename(get(stored, square)), info = stored)
  }
  expect_identical(formals(gf_squaresid), formals(gf_square_resid))
  expect_identical(body(gf_squaresid), body(gf_square_resid))
})

test_that("the alias draws exactly what the function it aliases draws", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  strip <- function(plot) {
    i <- layer_index(plot, "square_resid")
    layer <- plot$layers[[i]]
    list(
      i = i, geom = class(layer$geom)[[1]], stat = class(layer$stat)[[1]],
      position = class(layer$position)[[1]], inherit = layer$inherit.aes,
      aes_params = layer$aes_params, geom_params = layer$geom_params,
      mapping = vapply(layer$mapping, rlang::as_label, character(1)),
      built = ggplot2::ggplot_build(plot)$data[[i]],
      grob_x = as.numeric(ggplot2::layer_grob(plot, i)[[1]]$x)
    )
  }
  # an unseeded jitter is pinned with a fresh seed per call, so a jittered pair is only
  # comparable under the same seed
  jittered <- function(fn) {
    set.seed(7)
    suppressMessages(fn(gf_jitter(Thumb ~ Sex, data = Fingers, width = .1), model))
  }
  faceted <- function(fn) {
    suppressMessages(fn(
      gf_point(Thumb ~ Height, data = Fingers) %>% gf_facet_wrap(~RaceEthnic),
      lm(Thumb ~ Height, data = Fingers)
    ))
  }
  plain <- function(fn) {
    suppressMessages(fn(gf_point(Thumb ~ Sex, data = Fingers), model))
  }

  for (build in list(plain, jittered, faceted)) {
    expect_equal(strip(build(gf_squaresid)), strip(build(gf_square_resid)))
  }
})

test_that("a square resolves a mapping written in the frame that wrote it", {
  # `layer_factory()`'s generated `environment = parent.frame()` is what makes
  # this work, and it is forced from inside ggformula's `eval(pre)`: anything in
  # `pre` that resolves the name `environment` answers with ggformula's frame
  # instead of the caller's, and this mapping stops resolving
  model <- lm(Thumb ~ Height, data = Fingers)
  drawn_in_a_function <- function(fn) {
    local_color <- Fingers$Sex
    suppressMessages(fn(gf_point(Thumb ~ Height, data = Fingers), model, color = ~local_color))
  }

  for (fn in list(gf_square_resid, gf_squaresid)) {
    built <- ggplot2::ggplot_build(drawn_in_a_function(fn))
    expect_gt(length(unique(built$data[[2]]$colour)), 1)
  }
})

test_that("an unusable name given to a square is answered the way the caller's family does", {
  # a `gf_` function discards it in silence like `gf_point()` beneath it in the
  # pipe; a layer assembled the ggplot2 way warns on its own
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  expect_no_warning(suppressMessages(gf_square_resid(p, model, nonesuch = 1)))
  expect_no_warning(suppressMessages(gf_squaresid(p, model, nonesuch = 1)))

  fitted <- transform(Fingers, .fitted = stats::predict(model, Fingers))
  expect_warning(
    ggplot2::ggplot_build(
      p + ggplot2::layer(
        geom = GeomSquareResid, stat = StatResid, position = "identity",
        data = fitted, mapping = ggplot2::aes(yend = .data$.fitted),
        params = list(nonesuch = 1)
      )
    ),
    "unknown parameters"
  )
})
