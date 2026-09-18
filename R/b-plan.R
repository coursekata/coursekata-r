#' Build a data frame whose columns are named for the physical x/y aesthetics
#'
#' Every mark below is placed from a "predictor" coordinate and an "outcome"
#' coordinate, never from x/y directly, because which physical axis carries
#' which is a fact about the plot (`b_layer_spec()` reads it once, off the axis
#' the outcome is drawn on) and not about the mark. `cols`, built once per
#' call by `b_plan()`, says where each one lands; every mark-builder below
#' just says "predictor" and "outcome" and never asks which axis it is.
#'
#' @param cols A list with `predictor`, `outcome`, `predictor_end`,
#'   `outcome_end` -- each one of `"x"`, `"y"`, `"xend"`, `"yend"`.
#' @param predictor,outcome The coordinate to place at each end. `..._end`
#'   variants are only used by a segment.
#'
#' @return A data frame with columns literally named `x`, `y` (and `xend`,
#'   `yend` where supplied), regardless of `cols`.
#'
#' @noRd
b_mark_frame <- function(cols, predictor, outcome, predictor_end = NULL, outcome_end = NULL) {
  values <- list(predictor = predictor, outcome = outcome)
  wanted <- c("x", "y")
  if (!is.null(predictor_end)) {
    values$predictor_end <- predictor_end
    values$outcome_end <- outcome_end
    wanted <- c(wanted, "xend", "yend")
  }
  names(values) <- unlist(cols[names(values)])
  do.call(data.frame, c(values, stringsAsFactors = FALSE))[wanted]
}

#' A tagged segment mark, in the physical x/y a mark's plot uses
#'
#' @noRd
b_mark_segment <- function(tag, cols, predictor, outcome, predictor_end, outcome_end,
                           colour, linewidth, alpha = 1, arrow = NULL) {
  tag_layer(
    ggplot2::layer(
      geom = ggplot2::GeomSegment, stat = "identity", position = "identity",
      data = b_mark_frame(cols, predictor, outcome, predictor_end, outcome_end),
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend
      ),
      params = list(
        colour = colour, linewidth = linewidth, alpha = alpha, arrow = arrow, na.rm = TRUE
      ),
      inherit.aes = FALSE, show.legend = FALSE
    ),
    tag
  )
}

#' A tagged text mark, parsed as plotmath
#'
#' @noRd
b_mark_text <- function(tag, cols, predictor, outcome, label, colour, size,
                        predictor_just = 0.5, outcome_just = 0.5) {
  frame <- b_mark_frame(cols, predictor, outcome)
  frame$label <- label
  justification <- c(predictor = predictor_just, outcome = outcome_just)
  names(justification) <- unlist(cols[names(justification)])
  tag_layer(
    ggplot2::layer(
      geom = GeomBText, stat = "identity", position = "identity",
      data = frame,
      mapping = ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      params = list(
        colour = colour, size = size, parse = TRUE,
        x_just = justification[["x"]], y_just = justification[["y"]],
        na.rm = TRUE
      ),
      inherit.aes = FALSE, show.legend = FALSE
    ),
    tag
  )
}

#' Put a label a fixed glyph-relative distance along a semantic axis
#'
#' A positive direction places the label above or to the right of its anchor;
#' a negative direction places it below or to the left. `GeomBText` resolves
#' this separation at draw time, so the gap stays physical when the data range,
#' device size, scales, or predictor/outcome orientation changes.
#'
#' @noRd
b_outward_justification <- function(direction, gap = 0.35) {
  if (direction >= 0) -gap else 1 + gap
}

#' Retain the public continuous `label_nudge` as physical clearance
#'
#' The historical default is 0.08. Moving above or below it changes the
#' glyph-relative gap while leaving the label's data anchor on the coefficient
#' mark, where draw-time scale and coordinate handling can keep it in bounds.
#'
#' @noRd
b_continuous_label_gap <- function(label_nudge, default_gap) {
  max(0, default_gap + label_nudge - 0.08)
}

#' The hollow b0 dot on a continuous predictor's axis
#'
#' @noRd
b_mark_point <- function(tag, cols, predictor, outcome, colour, size) {
  tag_layer(
    ggplot2::layer(
      geom = ggplot2::GeomPoint, stat = "identity", position = "identity",
      data = b_mark_frame(cols, predictor, outcome),
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      params = list(
        shape = 21, colour = colour, fill = "white", size = size, stroke = 1, na.rm = TRUE
      ),
      inherit.aes = FALSE, show.legend = FALSE
    ),
    tag
  )
}

#' The power of ten `gf_b()` picks for a run, when the caller does not
#'
#' The label a run gets has to read cleanly -- "10 x b1", not "13.7 x b1" --
#' so the candidates are powers of ten, and the one chosen is
#' whichever sits within 5% to 80% of the predictor's span and closest to 10%
#' of it. Falls back to a raw 10% of the span when no power of ten qualifies
#' (a non-finite or non-positive span, or one so small the window between 5%
#' and 80% of it never captures a power of ten -- not observed for a positive
#' span, but the window is a computed fact, not a guarantee).
#'
#' @param x_span The predictor's own range (`max - min`), not the plot's.
#'
#' @return A single number.
#'
#' @noRd
nice_run <- function(x_span) {
  target <- 0.10 * x_span
  if (!is.finite(x_span) || x_span <= 0) {
    return(target)
  }
  lower <- 0.05 * x_span
  upper <- 0.80 * x_span
  exponent <- floor(log10(x_span))
  candidates <- 10^seq(exponent - 8, exponent + 8)
  qualifying <- candidates[candidates >= lower & candidates <= upper]
  if (length(qualifying) == 0) {
    return(target)
  }
  qualifying[[which.min(abs(qualifying - target))]]
}

#' The rise label's plotmath text
#'
#' @param run A run, in the predictor's own units.
#'
#' @return A string, meant to be parsed as plotmath.
#'
#' @noRd
format_run <- function(run) {
  if (isTRUE(all.equal(run, 1))) "b[1]" else paste0(run, " %*% b[1]")
}

#' Where to place the rise-over-run triangle
#'
#' Two candidate positions along the predictor's span -- 15% and 60% of the
#' way across it -- so there is always a fallback when a large `run` would
#' carry the far one past the data: a run at most 80% of the span (`nice_run`'s
#' own ceiling) always leaves the 15% position with room, because
#' 0.15 + 0.80 < 1. Among whichever candidates keep the whole triangle inside
#' the data's own range, the one farther from x = 0 wins, which is what keeps
#' the triangle off the b0 dot `show_b0` draws there.
#'
#' @param x_min,x_max The predictor's own range.
#' @param run The run the triangle spans.
#'
#' @return A single number, inside `[x_min, x_max]`.
#'
#' @noRd
b_run_x <- function(x_min, x_max, run) {
  span <- x_max - x_min
  candidates <- x_min + c(0.15, 0.60) * span
  fits <- candidates[candidates >= x_min & candidates + run <= x_max]
  if (length(fits) == 0) {
    fits <- candidates
  }
  fits[[which.max(abs(fits))]]
}

#' The b0 reference line and its label, for a categorical predictor's model
#'
#' `show_b0 = FALSE` drops both: a picture of b0 with no line and no label is
#' not a picture of anything, so there is nothing partial to keep.
#'
#' @noRd
b_ref_line <- function(cols, b0, args) {
  if (!isTRUE(args$show_b0)) {
    return(list())
  }
  list(
    b_mark_segment(
      "b0", cols, -Inf, b0, Inf, b0,
      colour = args$color, linewidth = args$b0_linewidth, alpha = args$b0_alpha
    ),
    b_mark_text(
      "b0_label", cols, (args$positions %||% 1)[[1L]] - args$label_nudge, b0, "b[0]",
      colour = args$label_color, size = args$label_size,
      predictor_just = 1, outcome_just = b_outward_justification(1)
    )
  )
}

#' The b0 reference line for the empty model, with its own label placement
#'
#' The empty model's one axis is a count axis, not a level axis, so
#' `b_ref_line()`'s `1 - label_nudge` -- a constant that only means "one group
#' apart" -- has no natural home here: it would land the label at count 0.92
#' regardless of how tall or wide the panel is. Placed at an infinite
#' predictor coordinate instead, the label sits at the panel's own edge no
#' matter the panel's size, and never needs to know a count axis's range to
#' do it -- ggplot2 drops an infinite coordinate from scale training, so this
#' cannot re-train the axis the way a finite coordinate would.
#'
#' @noRd
b_empty_marks <- function(cols, b0, args) {
  if (!isTRUE(args$show_b0)) {
    return(list())
  }
  label <- if (identical(cols$predictor, "y")) {
    # outcome on x (histogram, density, dotplot, boxplot): the b0 line is
    # vertical, so the label sits at the top of the panel, left of the line
    b_mark_text(
      "b0_label", cols, Inf, b0, "b[0]",
      colour = args$label_color, size = args$label_size,
      predictor_just = 1, outcome_just = 1
    )
  } else {
    # outcome on y: the b0 line is horizontal, so the label sits at the
    # panel's left edge, lifted just clear of the line
    b_mark_text(
      "b0_label", cols, -Inf, b0, "b[0]",
      colour = args$label_color, size = args$label_size,
      predictor_just = 0, outcome_just = b_outward_justification(1)
    )
  }
  list(
    b_mark_segment(
      "b0", cols, -Inf, b0, Inf, b0,
      colour = args$color, linewidth = args$b0_linewidth, alpha = args$b0_alpha
    ),
    label
  )
}

#' The b0 reference line plus one arrow per non-reference level
#'
#' Level order comes from `coef()`, never from `sort(levels(...))`. A factor's
#' contrasts are built in the order its levels were declared, and `coef()`
#' already reflects that order -- level `k`'s entry in `coef(lm(Tip ~
#' Condition))` is its difference from the reference regardless of which
#' level sorts first alphabetically -- so reading the arrows off `coefs`
#' directly is what keeps a releveled factor (`levels = c("treatment",
#' "control")`) drawing b1 on the arrow whose coefficient it is, not on
#' whichever level's name sorts first.
#'
#' @param coefs The full `coef(model)`, intercept included.
#'
#' @noRd
b_cat_marks <- function(cols, b0, coefs, args) {
  if (!is.null(args$run)) {
    warn(
      c(
        "`run` describes the slope of a continuous predictor",
        "*" = paste(
          "this model's predictor is categorical, so its coefficients are group",
          "differences, not a rate"
        )
      ),
      class = "coursekata_gf_b_run"
    )
  }

  marks <- b_ref_line(cols, b0, args)
  for (k in seq_along(coefs)[-1]) {
    b_k <- unname(coefs[[k]])
    arrow_x <- (args$positions %||% seq_along(coefs))[[k]] - args$arrow_nudge
    marks <- c(marks, list(
      b_mark_segment(
        paste0("bk_", k), cols, arrow_x, b0, arrow_x, b0 + b_k,
        colour = args$color, linewidth = args$arrow_linewidth,
        arrow = grid::arrow(length = grid::unit(0.1, "inches"), ends = "last")
      ),
      b_mark_text(
        paste0("bk_", k, "_label"), cols, arrow_x - args$label_nudge, b0 + b_k / 2,
        paste0("b[", k - 1, "]"),
        colour = args$label_color, size = args$label_size,
        predictor_just = 1
      )
    ))
  }
  marks
}

#' The rise-over-run triangle, plus the b0 dot at x = 0
#'
#' `run`/`run_x` fall back to `nice_run()`/`b_run_x()` when the caller does
#' not supply them. The run label stays on the triangle's interior side of the
#' run segment: below it for a positive rise and above it for a negative rise.
#' Label gaps use draw-time text justification rather than a fraction of a data
#' range, so the same placement works after scale reversal and when predictor
#' and outcome exchange physical axes.
#'
#' @param b1 The single slope coefficient, already unnamed.
#' @param values The predictor's own values (from the model's data), which
#'   set the span `run`/`run_x` are chosen against.
#'
#' @noRd
b_cont_marks <- function(cols, b0, b1, values, args) {
  finite <- values[is.finite(values)]
  x_min <- min(finite)
  x_max <- max(finite)
  x_span <- x_max - x_min

  run <- args$run %||% nice_run(x_span)
  run_x <- args$run_x %||% b_run_x(x_min, x_max, run)

  fit <- function(x) b0 + b1 * x
  y0 <- fit(run_x)
  y1 <- fit(run_x + run)
  rise <- y1 - y0
  away_from_run <- if (run >= 0) -1 else 1
  toward_rise <- if (rise >= 0) -1 else 1

  marks <- list(
    b_mark_segment(
      "b1", cols, run_x, y0, run_x, y1,
      colour = args$color, linewidth = args$arrow_linewidth,
      arrow = grid::arrow(length = grid::unit(0.05, "inches"), ends = "last")
    ),
    b_mark_text(
      "b1_label", cols, run_x, (y0 + y1) / 2,
      format_run(run), colour = args$label_color, size = args$label_size,
      predictor_just = b_outward_justification(
        away_from_run, b_continuous_label_gap(args$label_nudge, 0.35)
      )
    ),
    b_mark_segment(
      "run", cols, run_x, y1, run_x + run, y1,
      colour = args$color, linewidth = args$arrow_linewidth
    ),
    b_mark_text(
      "run_label", cols, run_x + run / 2, y1,
      as.character(run), colour = args$label_color, size = args$label_size,
      predictor_just = if (run >= 0) 0 else 1,
      outcome_just = b_outward_justification(toward_rise, gap = 0.8)
    )
  )

  if (isTRUE(args$show_b0)) {
    label_direction <- if (0 <= x_min) {
      1
    } else if (0 >= x_max) {
      -1
    } else if (run_x >= 0) {
      -1
    } else {
      1
    }
    marks <- c(marks, list(
      b_mark_point("b0", cols, 0, b0, colour = args$color, size = args$b0_size),
      b_mark_text(
        "b0_label", cols, 0, b0, "b[0]",
        colour = args$label_color, size = args$label_size,
        predictor_just = b_outward_justification(
          label_direction, b_continuous_label_gap(args$label_nudge, 1.2)
        )
      )
    ))
  }
  marks
}

#' Decide what `gf_b()` draws for a model, and build the marks that draw it
#'
#' Reads the model's coefficients once, then dispatches to the categorical or
#' continuous mark-builder. `kind` here is this function's own vocabulary --
#' `"categorical"`, `"continuous"` or `"empty"` -- unrelated to
#' `model_plan()`'s `"line"`/`"segment"`/`"hline"`/`"vline"`, which names a
#' geom rather than a predictor's type.
#'
#' @param outcome_axis `"x"` or `"y"`: which axis the model's outcome is on.
#' @param predictor The predictor's column name, or `NULL` for an empty model.
#' @param categorical `TRUE`/`FALSE`/`NA` (`NA` when there is no predictor).
#' @param values The predictor's own values, from the model's data.
#' @param coefs `coef(model)`, intercept included.
#' @param args The extras `gf_b()`/`gf_coef()` were called with.
#'
#' @return A list with `kind`, `coefs` and `marks` (tagged layers).
#'
#' @noRd
b_plan <- function(outcome_axis, predictor, categorical, values, coefs, args) {
  predictor_axis <- setdiff(c("x", "y"), outcome_axis)
  cols <- list(
    predictor = predictor_axis, outcome = outcome_axis,
    predictor_end = paste0(predictor_axis, "end"), outcome_end = paste0(outcome_axis, "end")
  )
  b0 <- unname(coefs[[1]])
  kind <- if (is.null(predictor)) {
    "empty"
  } else if (isTRUE(categorical)) {
    "categorical"
  } else {
    "continuous"
  }

  marks <- switch(kind,
    empty = b_empty_marks(cols, b0, args),
    categorical = b_cat_marks(cols, b0, coefs, args),
    continuous = b_cont_marks(cols, b0, unname(coefs[[2]]), values, args)
  )

  list(kind = kind, coefs = coefs, marks = marks)
}

#' Refuse a fit whose coefficients are not the ones the marks assume
#'
#' Shared by both paths, and placed after them, because the inferred fit is
#' subject to the same two assumptions as one the reader handed in: a global
#' `options(contrasts = )` reaches a model this function fit for itself just as
#' surely as one it was given.
#'
#' TWO ASSUMPTIONS, both silent when they fail, which is what makes them worth
#' refusing rather than warning about.
#'
#' `b0` is the first coefficient. Without an intercept there is no `b0` at all
#' and `coef()` starts at the slope, so every mark measured from `b0` -- the
#' reference line, the rise, each arrow's foot -- is measured from the wrong
#' number, and a one-predictor fit dies outright reaching past the end of a
#' one-element vector for its slope.
#'
#' Coefficient `k` belongs to group `k`. That is true of treatment coding and
#' of nothing else. Changing which level is the reference is fine, because the
#' plot orders its groups by the same factor the model coded, and there is a
#' test that holds that. But `contr.sum` makes the intercept a grand mean and
#' each coefficient a deviation, `contr.helmert` makes it a running comparison,
#' and an ordered factor gets `contr.poly`, whose terms are a linear trend and
#' a quadratic one -- no arrangement of which is "the difference for group k".
#' Each of those still draws an arrow of some length at some group, and none of
#' them is a picture of what the model says.
#'
#' Treatment coding reports itself as the character string `"contr.treatment"`;
#' every other scheme, including `contr.treatment` with a `base` other than the
#' first level, arrives as a matrix. So the accepted case is the narrow one,
#' named outright.
#'
#' @param fit The model whose coefficients will be drawn.
#' @param predictor The predictor's term, or `NULL` for the empty model.
#' @param categorical `TRUE`/`FALSE`/`NA`, as `b_plan()` reads it.
#' @param fn The name to refuse in, `"gf_b"` or `"gf_coef"`.
#' @param call The calling environment, for error reporting.
#'
#' @return `fit`, invisibly.
#'
#' @noRd
check_b_coefficients <- function(fit, predictor, categorical, fn, call = caller_env()) {
  if (identical(as.integer(attr(stats::terms(fit), "intercept")), 0L)) {
    abort(
      c(
        glue("`{fn}()` annotates a model's coefficients starting from b0"),
        x = paste(
          "this model was fit without an intercept, so it has no b0 and its",
          "coefficients start at b1"
        ),
        "*" = "the reference line, the rise, and every arrow's foot are all measured from b0",
        i = "fit the model with its intercept"
      ),
      call = call
    )
  }

  if (!isTRUE(categorical)) {
    return(invisible(fit))
  }

  check_b_coding(fit$contrasts[[predictor]], predictor, fn, call)
  invisible(fit)
}

#' Check the interpretation used by group-difference arrows
#' @noRd
check_b_coding <- function(coding, predictor, fn, call = caller_env()) {
  if (identical(coding, "contr.treatment")) {
    return(invisible(NULL))
  }

  named <- if (is.character(coding)) glue("`{coding}`") else "a contrast matrix of its own"
  abort(
    c(
      glue("`{fn}()` draws each coefficient as one group's difference from the reference group"),
      x = glue("this model codes `{predictor}` with {named}, where a coefficient is not that"),
      "*" = paste(
        "under any other coding an arrow would still be drawn, at a group, with a length --",
        "and it would not be the number the model reports for that group"
      ),
      i = paste(
        "fit the predictor with treatment coding;",
        "`relevel()` chooses which group is the reference"
      )
    ),
    call = call
  )
}

#' Refuse a model whose predictor the plot does not draw
#'
#' `resid_end()` is the same guard for the OUTCOME, and every mark here needs
#' both. A coefficient annotation is placed from the coefficients and from
#' level indices, never from a drawn point, so nothing about drawing it
#' notices that the numbers belong to some other variable: `lm(Thumb ~ Sex)`
#' will happily put its two-group arrow over a plot of five race groups, and
#' `lm(Thumb ~ log(Height))` will put its rise-over-run triangle at x = 4.23 on
#' an axis that runs 59 to 76.5. Both draw a picture that is not wrong-looking,
#' which is the reason to refuse rather than warn.
#'
#' Compared as the reader SPELLED them, not as columns. `Height` and
#' `log(Height)` are the same column and different axes, and it is the axis a
#' mark lands on: b1 is a rise per unit of whatever the model was fit on, so
#' the triangle is only true where the plot measures that same thing. Spelling
#' them alike is also what makes a basis expansion refuse itself -- a plot has
#' no `poly(Height, 2)` axis to draw one on, and a model with more coefficients
#' than the triangle has sides would otherwise drop them silently.
#'
#' @param spec A `plot_spec()` list.
#' @param outcome_axis The aesthetic carrying the model's outcome.
#' @param predictor The model's single predictor, as its term is spelled, or
#'   `NULL` for the empty model -- which predicts the same number everywhere
#'   and so is drawable over any predictor at all.
#' @param fn The name to refuse in, `"gf_b"` or `"gf_coef"`.
#' @param call The calling environment, for error reporting.
#'
#' @return `spec`, invisibly.
#'
#' @noRd
check_b_predictor <- function(spec, outcome_axis, predictor, fn, call = caller_env()) {
  if (is.null(predictor)) {
    return(invisible(spec))
  }

  axis <- setdiff(c("x", "y"), outcome_axis)
  drawn <- if (axis %in% names(spec$axes)) spec$axes[[axis]] else NULL
  if (identical(drawn, predictor)) {
    return(invisible(spec))
  }

  abort(
    c(
      glue("`{fn}()` annotates a model of what the plot draws"),
      x = if (is.null(drawn)) {
        glue(
          "the model predicts from `{predictor}`, ",
          "and this plot has no {axis} axis to draw it on"
        )
      } else {
        glue("the model predicts from `{predictor}`, and the plot's {axis} axis draws `{drawn}`")
      },
      "*" = paste(
        "every mark is placed from the coefficients rather than from the points, so",
        "they would land at values this axis does not measure"
      ),
      i = glue("plot the predictor this model uses, or annotate the model this plot was built for")
    ),
    call = call
  )
}

#' Select the coefficient claim without resolving source data
#'
#' Addition inspects only mappings, model specifications, and any supplied fit.
#' Data callbacks, fitting, scales, and coordinates are resolved by the public
#' layer/stat lifecycle. A faceted plot requires an explicit fit because its
#' panels can imply different models.
#'
#' @param object The plot the layer is being added to.
#' @param model A model already fit, or `NULL` to read the plot's implied one.
#' @param args The extras `gf_b()`/`gf_coef()` were called with.
#' @param fn The name to refuse and warn in, `"gf_b"` or `"gf_coef"`.
#' @param call The calling environment, for error reporting.
#'
#' @return A specification for `b_defer_spec()` to bind to the plot.
#'
#' @noRd
b_layer_spec <- function(object, model, args, fn, call = caller_env(), source_is_local = FALSE) {
  check_resid_plot(object, fn, call = call)
  object <- stabilize_source_data(object)

  if (!inherits(object$coordinates, "CoordCartesian")) {
    abort(
      c(
        glue("`{fn}()` needs a plot with cartesian x and y axes"),
        glue("this plot uses {class(object$coordinates)[[1]]}")
      ),
      class = "coursekata_gf_b_coord",
      call = call
    )
  }

  # Every extra here sets a mark's appearance to one value. `color = ~species`
  # is the family's mapping idiom everywhere else, and `layer_factory()` would
  # turn it into a mapping -- but `pre` reads these before that conversion, and
  # a formula handed to `ggplot2::layer(params = )` dies inside vctrs at
  # render. Refuse it in words instead. Mapping it is not the alternative:
  # `gf_b_layer_fun()` discards everything ggformula assembles, so an aesthetic
  # that got through would be silently dropped.
  mapped <- names(args)[vapply(args, function(x) is_formula(x) && length(x) == 2L, logical(1))]
  if (length(mapped) > 0) {
    arg <- mapped[[1]]
    example <- if (grepl("color$", arg)) '"red"' else "1"
    abort(
      c(
        glue("`{fn}()` takes a value for each mark's look, not a mapping"),
        x = glue("`{arg} = {deparse1(args[[arg]])}` is a formula"),
        i = paste(
          "each mark is one row computed from the coefficients, so there are",
          "no rows of data to map an aesthetic over"
        ),
        i = glue("give one value: `{fn}(model, {arg} = {example})`")
      ),
      call = call
    )
  }

  if (is_formula(model)) {
    abort(
      c(
        glue("`{fn}()` annotates a model that has been fit"),
        x = glue("`{deparse1(model)}` is a formula, not a fit"),
        i = glue("fit it first: `{fn}(lm(Thumb ~ Height, data = Fingers))`"),
        i = "or leave it out, and the model the plot implies is used"
      ),
      call = call
    )
  }

  source <- plot_source(object, resolve.data = FALSE)
  mapping <- source$mapping
  axes <- mapping[intersect(c("x", "y"), names(mapping))]
  axes <- axes[!vapply(axes, function(q) has_build_time_call(quo_get_expr(q)), logical(1))]
  if (!length(axes)) {
    check_model_axes(list(axes = list()), fn = fn, call = call)
  }
  model_layers <- which(vapply(object$layers, function(layer) {
    inherits(layer$stat, "StatModel")
  }, logical(1)))
  if (source_is_local) model_layers <- integer()
  if (is.null(model) && length(model_layers) > 1L) {
    abort(glue("`{fn}()` needs one model to annotate; pass the fitted `model`."), call = call)
  }
  if (is.null(model) && length(model_layers) == 1L) {
    layer <- object$layers[[model_layers]]
    if (!inherits(layer$position, "PositionIdentity")) {
      abort(glue("`{fn}()` requires an identity model position; a position adjustment moves the fitted claim."), call = call)
    }
    model <- model_layer_fit(layer)
  }
  outcome_axis <- predictor <- categorical <- coefs <- NULL
  if (is.null(model)) {
    if (length(object$facet$vars())) {
      abort(glue("`{fn}()` needs a fitted model on a faceted plot."), call = call)
    }
    if (length(model_layers)) {
      layer <- object$layers[[model_layers]]
      if (".model_kind" %in% names(layer$mapping)) {
        abort(glue("`{fn}()` needs the fitted model for this prepared prediction grid."), call = call)
      }
      formula <- layer$stat_params$formula %||% (y ~ x)
      if (!identical(all.vars(formula), c("y", "x")) ||
          !identical(attr(stats::terms(formula), "term.labels"), "x") ||
          attr(stats::terms(formula), "intercept") != 1L) {
        abort(glue("`{fn}()` needs a linear model with an intercept to draw one slope."), call = call)
      }
    }
  } else {
    if (!inherits(model, "lm") || inherits(model, "glm")) {
      abort(glue("`{fn}()` needs a fitted `lm()` or `aov()` model."), call = call)
    }
    frame <- model$model
    terms <- stats::terms(model)
    predictors <- names(frame)[!names(frame) %in% c(names(frame)[[1L]], "(weights)")]
    if (length(attr(terms, "offset"))) {
      abort(glue("`{fn}()` cannot annotate a model with an offset as one coefficient claim."), call = call)
    }
    if (length(predictors) > 1L) {
      abort(glue("`{fn}()` annotates a model with one predictor; this model has {length(predictors)}."), call = call)
    }
    labels <- vapply(axes, source_mapping_label, character(1))
    pins <- plot_pins(object)
    for (axis in intersect(names(labels), names(pins))) {
      labels[[axis]] <- source_mapping_label(pins[[axis]])
    }
    outcome <- names(frame)[[1L]]
    outcome_axis <- names(labels)[labels == outcome]
    predictor <- if (length(predictors)) predictors else NULL
    categorical <- if (is.null(predictor)) NA else !is.numeric(frame[[predictor]])
    check_b_coefficients(model, predictor, categorical, fn, call)
    if (!is.null(predictor) && (!identical(attr(terms, "term.labels"), predictor) ||
        (!categorical && (is.matrix(frame[[predictor]]) || length(stats::coef(model)) != 2L)))) {
      abort(glue("`{fn}()` needs one linear slope; `{predictor}` has a different coefficient structure."), call = call)
    }
    coefs <- stats::coef(model)
    if (any(!is.finite(coefs))) {
      abort(glue("`{fn}()` needs finite coefficients."), call = call)
    }
    if (isTRUE(categorical) && !is.null(args[["run"]])) {
      # The claim's type is known without touching any plot rows.
      b_cat_marks(list(predictor = "x", outcome = "y", predictor_end = "xend",
                       outcome_end = "yend"), 0, c(0, 1), args)
      args["run"] <- list(NULL)
    }
  }
  list(plot = object, source = source, model = model, model_index = model_layers,
       outcome_axis = outcome_axis, predictor = predictor, categorical = categorical,
       coefs = coefs, args = args, fn = fn, call = call)
}
