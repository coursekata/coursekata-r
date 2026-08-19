#' Build a data frame whose columns are named for the physical x/y aesthetics
#'
#' Every mark below is placed from a "predictor" coordinate and an "outcome"
#' coordinate, never from x/y directly, because which physical axis carries
#' which is a fact about the plot (`gf_b_spec()` reads it once, off the axis
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
                        hjust = 0.5, vjust = 0.5) {
  frame <- b_mark_frame(cols, predictor, outcome)
  frame$label <- label
  tag_layer(
    ggplot2::layer(
      geom = ggplot2::GeomText, stat = "identity", position = "identity",
      data = frame,
      mapping = ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      params = list(
        colour = colour, size = size, parse = TRUE, hjust = hjust, vjust = vjust, na.rm = TRUE
      ),
      inherit.aes = FALSE, show.legend = FALSE
    ),
    tag
  )
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

#' The b0 reference line and its label -- drawn the same way whether the model
#' has no predictor or a categorical one
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
      "b0_label", cols, 1 - args$label_nudge, b0, "b[0]",
      colour = args$label_color, size = args$label_size, hjust = 1
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
      colour = args$label_color, size = args$label_size, hjust = 1, vjust = 1
    )
  } else {
    # outcome on y: the b0 line is horizontal, so the label sits at the
    # panel's left edge, lifted just clear of the line
    b_mark_text(
      "b0_label", cols, -Inf, b0, "b[0]",
      colour = args$label_color, size = args$label_size, hjust = 0, vjust = -0.3
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
    arrow_x <- k - args$arrow_nudge
    marks <- c(marks, list(
      b_mark_segment(
        paste0("bk_", k), cols, arrow_x, b0, arrow_x, b0 + b_k,
        colour = args$color, linewidth = args$arrow_linewidth,
        arrow = grid::arrow(length = grid::unit(0.1, "inches"), ends = "last")
      ),
      b_mark_text(
        paste0("bk_", k, "_label"), cols, arrow_x - args$label_nudge, b0 + b_k / 2,
        paste0("b[", k - 1, "]"),
        colour = args$label_color, size = args$label_size, hjust = 1
      )
    ))
  }
  marks
}

#' The rise-over-run triangle, plus the b0 dot at x = 0
#'
#' `run`/`run_x` fall back to `nice_run()`/`b_run_x()` when the caller does
#' not supply them. The run label sits under the horizontal run segment for a
#' positive rise and over it for a negative one, moved toward the arrow's own
#' body rather than away from it, which is what "under (or over) the segment"
#' means for a segment that is sometimes the top of the rise and sometimes the
#' bottom.
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
  toward_start <- if (rise >= 0) 1 else -1

  marks <- list(
    b_mark_segment(
      "b1", cols, run_x, y0, run_x, y1,
      colour = args$color, linewidth = args$arrow_linewidth,
      arrow = grid::arrow(length = grid::unit(0.1, "inches"), ends = "last")
    ),
    b_mark_text(
      "b1_label", cols, run_x - args$label_nudge * x_span, (y0 + y1) / 2,
      format_run(run), colour = args$label_color, size = args$label_size, hjust = 1
    ),
    b_mark_segment(
      "run", cols, run_x, y1, run_x + run, y1,
      colour = args$color, linewidth = args$arrow_linewidth
    ),
    b_mark_text(
      "run_label", cols, run_x + run / 2, y1 - toward_start * abs(rise) * 0.12,
      as.character(run), colour = args$label_color, size = args$label_size, vjust = 1
    )
  )

  if (isTRUE(args$show_b0)) {
    marks <- c(marks, list(
      b_mark_point("b0", cols, 0, b0, colour = args$color, size = args$b0_size),
      b_mark_text(
        "b0_label", cols, -args$label_nudge * x_span, b0, "b[0]",
        colour = args$label_color, size = args$label_size, hjust = 1
      ),
      do.call(ggplot2::expand_limits, stats::setNames(list(0), cols$predictor))
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
#' @return A list with `kind`, `coefs` and `marks` (a list of tagged layers
#'   and, on a continuous model with `show_b0 = TRUE`, an `expand_limits()`).
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

  coding <- fit$contrasts[[predictor]]
  if (identical(coding, "contr.treatment")) {
    return(invisible(fit))
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

#' Translate a plot and a model into `gf_b()`'s marks
#'
#' The counterpart of `model_layer_spec()` and `resid_spec()`: everything
#' `layer_factory()`'s `pre` has to shadow, decided in one function.
#'
#' With no model, this reads the SAME decision `gf_model()`'s inference reads
#' (`implied_model()`), so the two features never disagree about which axis
#' carries the outcome or what shape the predictor is -- only the fit differs,
#' because `gf_b()` needs `coef()` at call time to place an arrow, and
#' `implied_model()` deliberately produces no fit at all, so it can be shared
#' with `gf_model()`'s per-panel stat. `gf_b()` fits its own
#' `stats::lm()` on the pinned plot's whole data, which is exactly why it
#' refuses on a faceted plot: a per-panel `gf_model()` line next to whole-data
#' arrows would show two different fits with no way to tell them apart.
#'
#' With a model, the outcome-axis check reuses `resid_end()`'s wording
#' verbatim -- a residual and a coefficient are both read along the axis
#' carrying the model's outcome, so the same condition names the same problem
#' the same way.
#'
#' @param object The plot the layer is being added to.
#' @param model A model already fit, or `NULL` to read the plot's implied one.
#' @param args The extras `gf_b()`/`gf_coef()` were called with.
#' @param fn The name to refuse and warn in, `"gf_b"` or `"gf_coef"`.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with `plot` (the plot to add the marks to -- pinned, on the
#'   inferred path) and `marks` (from `b_plan()`).
#'
#' @noRd
gf_b_spec <- function(object, model, args, fn, call = caller_env()) {
  check_resid_plot(object, fn, call = call)

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

  if (is.null(model)) {
    implied <- implied_model(object, fn = fn, call = call)
    if (length(implied$facets) > 0) {
      n <- nrow(ggplot2::ggplot_build(implied$plot)$layout$layout)
      abort(
        c(
          glue("`{fn}()` needs to be told which model to annotate on a faceted plot"),
          x = glue("this plot has {n} panels, and the coefficients differ from panel to panel"),
          i = glue("fit the model and pass it: `{fn}(lm(Thumb ~ Height, data = Fingers))`")
        ),
        call = call
      )
    }

    fit <- stats::lm(implied$formula, implied$data)
    outcome_axis <- if (implied$flipped) "x" else "y"
    predictor <- if (is.null(implied$predictor)) NULL else implied$predictor$column
    categorical <- if (is.null(predictor)) NA else identical(implied$kind, "segment")
    values <- if (is.null(predictor)) NULL else implied$data[[predictor]]
    plot <- implied$plot
  } else {
    spec <- plot_spec(object)
    frame <- model$model
    predictors <- names(frame)[-1]
    if (length(predictors) > 1) {
      abort(
        c(
          glue("`{fn}()` annotates a model with one predictor"),
          x = glue("this model has {length(predictors)}: {collapse(predictors)}"),
          "*" = paste(
            "with two predictors there is no single rise to draw, because b1 is the",
            "slope holding the other predictor constant"
          )
        ),
        call = call
      )
    }

    end <- resid_end(spec, model, call = call)
    outcome_axis <- if (identical(end, "xend")) "x" else "y"
    predictor <- if (length(predictors) == 1) predictors else NULL
    check_b_predictor(spec, outcome_axis, predictor, fn, call = call)
    categorical <- if (is.null(predictor)) NA else !is.numeric(frame[[predictor]])
    values <- if (is.null(predictor)) NULL else frame[[predictor]]
    fit <- model
    plot <- object
  }

  coefs <- stats::coef(fit)
  check_b_coefficients(fit, predictor, categorical, fn, call = call)
  plan <- b_plan(outcome_axis, predictor, categorical, values, coefs, args)
  list(plot = plot, marks = plan$marks, kind = plan$kind, coefs = plan$coefs)
}

#' Build the layer function that adds every mark `gf_b()` computed
#'
#' `gf_b_spec()` has already decided everything there is to decide, so the
#' generated closure ignores the geom/stat/position/params/mapping/data
#' `layer_factory()` would otherwise assemble and returns `marks` outright.
#' Unlike `model_layer_fun()`, which draws one geom and so has one params bag
#' for ggformula's own arguments to join, `gf_b()` draws several different
#' geoms (segment, text, point) from one call, and no single params bag could
#' belong to all of them -- so nothing ggformula assembled from the caller's
#' `...` can be forwarded here; `gf_b_warn_unreachable()` is what tells the
#' caller so. `layer_factory()` composes a list with `+` (`squareplot_layer()`
#' does the same).
#'
#' @param marks A list of tagged layers and `expand_limits()`, from
#'   `gf_b_spec()`.
#'
#' @return A function with the formals `layer_factory()` expects.
#'
#' @noRd
gf_b_layer_fun <- function(marks) {
  force(marks)
  function(geom, stat, position, params = NULL, mapping = NULL, data = NULL, ...) marks
}

#' Warn when the caller wrote something no mark `gf_b()` draws can honor
#'
#' `gf_b_layer_fun()` returns `marks` outright and never assembles a
#' `ggplot2::layer()` from ggformula's own `params`/`mapping`/`...`, so an
#' argument like `alpha` or `linetype`, or a non-default `show.legend`,
#' silently does nothing -- the marks are annotations, not a mapped geom, and
#' have no legend to contribute to. This turns that silence into a warning,
#' per the project's default/override/warn rule, rather than leaving a
#' reader's `gf_b(m, alpha = .2)` do nothing with no sign why.
#'
#' @param dots Named list, the caller's `...`, with the British spellings
#'   `colour`/`label_colour` already read out of it by the caller.
#' @param show_legend The caller's `show.legend`, `NA` when never set.
#' @param fn The name to warn in, `"gf_b"` or `"gf_coef"`.
#'
#' @return `NULL`, invisibly.
#'
#' @noRd
gf_b_warn_unreachable <- function(dots, show_legend, fn) {
  unreachable <- names(dots)
  if (!isTRUE(is.na(show_legend))) {
    unreachable <- c(unreachable, "show.legend")
  }
  if (length(unreachable) == 0) {
    return(invisible(NULL))
  }
  warn(
    c(
      glue(
        "`{fn}()` places its own marks, so {collapse(paste0('`', unreachable, '`'))} ",
        "cannot reach them"
      ),
      "i" = paste(
        "set appearance with `color`, `label_color`, `label_size`,",
        "`arrow_linewidth`, `b0_linewidth`, `b0_size`, `b0_alpha`"
      )
    ),
    class = "coursekata_gf_b_unreachable"
  )
  invisible(NULL)
}

#' Annotate a model's coefficients on a plot
#'
#' Draws the intercept and slope (or group differences) of a fitted model as
#' arrows and labels directly on the plot they describe: a rise-over-run
#' triangle for a continuous predictor, one arrow per group for a categorical
#' one. Where [gf_model()] draws the fit itself, `gf_b()` draws the numbers
#' that describe it.
#'
#' @details
#' # What is drawn
#'
#' **A continuous predictor**: a vertical rise arrow from `fit(run_x)` to
#' `fit(run_x + run)`, a horizontal run segment at its tip, a rise label
#' (plotmath b1 when `run` is 1, otherwise `run` times b1), a run-distance
#' label under the run segment (over it, for a negative rise), and a hollow
#' dot at `(0, b0)` with a b0 label.
#'
#' **A categorical predictor** (`k` levels): one horizontal reference line at
#' `b0` (the reference level's mean), and for each `k >= 2` a segment from
#' `b0` to `b0 + b_k` with an arrow head, labelled (plotmath) b0, b1, … Level
#' order is read off `coef(model)`, so a releveled factor still labels the
#' arrow that matches its coefficient.
#'
#' **No predictor** (the empty model): the `b0` line and its label, nothing
#' else.
#'
#' Every mark is a separately tagged layer -- `"b0"`, `"b1"`, `"bk_2"`,
#' `"run"`, and each one's own `"_label"` -- so a script can find one without
#' counting layers.
#'
#' # No model
#'
#' With no `model`, `gf_b()` reads the model the plot implies -- the same
#' decision [gf_model()]'s own inference reads -- and fits it at call time, on
#' the plot's whole data. This is refused on a faceted plot, because
#' `gf_model()`'s inferred line is fit per panel and a single set of arrows
#' drawn over it would describe a fit no panel actually has.
#'
#' # Placement
#'
#' Every mark is placed from the model's coefficients and from level indices,
#' never from a drawn point's position, so jitter never moves an arrow.
#'
#' `show_b0 = TRUE` (the default) expands the x axis to include 0 on a
#' continuous model, because b0 is the value at `x = 0` and a picture of it
#' that does not show `x = 0` is not a picture of b0. Calling `gf_lims(x = )`
#' afterward overrides that expansion and can push the b0 dot off the page.
#'
#' @param object A plot created with the `ggformula` package.
#' @param model The model to annotate: a fit from [`lm()`] or [`aov()`], with
#'   one predictor at most. A formula is refused -- `gf_b()`'s whole output is
#'   a set of labelled numbers, and there is no fit to read them from. May be
#'   given positionally or as `model =`. Omitted, the model the plot implies
#'   is fit and annotated instead.
#' @param color,label_color The arrows/lines and the label text. `colour` and
#'   `label_colour` are accepted too. Each is a single value, not a mapping --
#'   every mark is one row computed from the coefficients, so there are no
#'   rows of data to map an aesthetic over; `color = ~variable` is refused.
#' @param label_size,arrow_linewidth,b0_linewidth,b0_size Sizes for the labels,
#'   the arrows, the b0 line and the b0 dot.
#' @param show_b0 Draw the `b0` line/dot and its label, and expand the x axis
#'   to include 0 on a continuous model. `TRUE` by default; a later
#'   `gf_lims(x = )` overrides the expansion and can push the b0 dot off the
#'   page.
#' @param run,run_x The run a continuous model's rise is measured over, and
#'   the x position the triangle starts at. Both chosen from the data when
#'   left `NULL`. Naming `run` on a categorical model is warned about and
#'   ignored -- its coefficients are group differences, not a rate.
#' @param b0_alpha The transparency of the categorical b0 reference line.
#' @param arrow_nudge,label_nudge A categorical arrow's x position, and its
#'   label's offset from it, in level units (1 = one group apart). Not used on
#'   the empty model, whose one axis is a count, not a level, and whose b0
#'   label is placed at the panel's edge instead.
#' @param gformula Not used. `gf_b()` annotates a model, not an aesthetic
#'   formula; a model given positionally lands here and is moved to `model`.
#' @param data Not used. The marks are placed from the model's own
#'   coefficients and data.
#' @param ... Not used. Every mark states its own geom and params; set
#'   appearance with `color`, `label_color`, `label_size`, `arrow_linewidth`,
#'   `b0_linewidth`, `b0_size`, `b0_alpha`. Anything else here (`alpha`,
#'   `linetype`, ...) is warned about and dropped, because the marks are
#'   heterogeneous geoms with no single params bag to receive it.
#' @param xlab,ylab,title,subtitle,caption Labels for the plot.
#' @param geom,stat,position Not set by the caller. Every mark states its own
#'   geom.
#' @param show.legend Not used. The marks are annotations and never
#'   contribute to a legend; a non-default value is warned about and dropped.
#' @param show.help Print the function's own help instead of drawing.
#' @param inherit Not set by the caller. Every mark states its own aesthetics.
#' @param environment The environment mappings are resolved in.
#'
#' @return A ggplot object with the model's coefficients annotated on it.
#'
#' @seealso [gf_model()] draws the fit itself.
#'
#' @export
#' @importFrom ggformula layer_factory
#' @examples
#' # continuous: b1 as a rise-over-run triangle, b0 where the line meets x = 0
#' height_model <- lm(Thumb ~ Height, data = Fingers)
#' gf_point(Thumb ~ Height, data = Fingers, alpha = .3) %>% gf_b(height_model)
#'
#' # the slope per one unit
#' gf_point(Thumb ~ Height, data = Fingers) %>% gf_b(height_model, run = 1)
#'
#' # an explicit run labels the rise "10 x b1"
#' gf_point(Thumb ~ Height, data = Fingers) %>% gf_b(height_model, run = 10)
#'
#' # categorical: b0 is the reference group's mean, each b_k is an arrow to group k
#' tip_model <- lm(Tip ~ Condition, data = TipExperiment)
#' gf_jitter(Tip ~ Condition, data = TipExperiment, width = .1) %>% gf_b(tip_model)
#'
#' # no model: the model the plot implies, on the values the plot drew
#' set.seed(1)
#' gf_jitter(shuffle(Height) ~ Sex, data = Fingers, width = .1) %>%
#'   gf_model() %>%
#'   gf_b()
#'
#' # gf_coef() is the same function under the name coef() readers look for
#' flipper_model <- lm(body_mass_kg ~ flipper_length_m, data = penguins)
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins) %>%
#'   gf_coef(flipper_model)
gf_b <- ggformula::layer_factory(
  geom = ggplot2::GeomSegment, stat = "identity", position = "identity",
  aes_form = NULL,
  extras = alist(
    model = , color = "#b599ed", label_color = "black", label_size = 3.5,
    arrow_linewidth = 0.5, show_b0 = TRUE, run = NULL, run_x = NULL,
    b0_alpha = 0.3, b0_linewidth = 0.8, b0_size = 4,
    arrow_nudge = 0.18, label_nudge = 0.08
  ),
  note = "the model whose coefficients to annotate: a fit from lm() or aov()",
  # `pre` is evaluated in the ggformula namespace, so a coursekata helper needs :::
  pre = {
    # `layer_factory()` binds the second positional argument to `gformula`, but
    # `gf_b()` takes a model there, not an aesthetic formula. See `gf_model()`
    # for the full reasoning; the move is unconditional.
    if (!missing(gformula) && missing(model)) {
      model <- gformula
      gformula <- NULL
    }

    # `pre` runs ahead of the help gate on every supported release, so a bare
    # `gf_b()` has to fall straight through to it. Base `missing()`, never
    # `rlang::is_missing()`, which forces the promise and kills that path;
    # `isTRUE()` because `show.help` is NULL, not FALSE, until ggformula
    # decides one. See `gf_resid()` for the full reasoning.
    if ((!missing(object) || !missing(model)) && !isTRUE(show.help)) {
      dots <- list(...)
      color <- dots$colour %||% color
      label_color <- dots$label_colour %||% label_color
      unreachable_dots <- dots[setdiff(names(dots), c("colour", "label_colour"))]
      args <- list(
        color = color, label_color = label_color, label_size = label_size,
        arrow_linewidth = arrow_linewidth, show_b0 = show_b0, run = run, run_x = run_x,
        b0_alpha = b0_alpha, b0_linewidth = b0_linewidth, b0_size = b0_size,
        arrow_nudge = arrow_nudge, label_nudge = label_nudge
      )
      coursekata:::gf_b_warn_unreachable(unreachable_dots, show.legend, "gf_b")
      spec <- coursekata:::gf_b_spec(
        object, if (missing(model)) NULL else model, args, "gf_b"
      )
      object <- spec$plot
      layer_fun <- coursekata:::gf_b_layer_fun(spec$marks)
    }
  }
)

#' @rdname gf_b
#' @description
#' `gf_coef()` is a fully supported alias of `gf_b()`. The package already
#' exports `b()`, `b0()`, `b1()` as its vocabulary for coefficients, and a
#' reader who knows [`stats::coef()`] will look for a plot-side counterpart
#' under that name.
#' @export
#
# Generated, not forwarded, and not a second binding of the same closure.
#
# A forwarder loses the caller's name: measured elsewhere in this package
# (`gf_squaresid()`'s comment, `R/gf_resid_gf_squaresid.R`), a forwarder
# reports the wrong function name in every refusal and in the bare-call help
# header, and its `environment = parent.frame()` resolves to the forwarder's
# own frame, so a mapped aesthetic written from inside a function stops
# resolving. Generating the alias instead needs no stack introspection at
# all, and keeps this file's two refusal names literal the way `gf_resid()`'s
# is.
#
# THE PRICE IS DRIFT, AND IT IS PAID BY A TEST. Everything below is
# `gf_b()`'s factory call with one string changed, and `test-gf_b.R` asserts
# exactly that by comparing every argument `layer_factory()` stored on the
# two closures, `pre` included, after a mechanical rename -- so an edit made
# to one and not the other fails the suite rather than reaching a reader.
gf_coef <- ggformula::layer_factory(
  geom = ggplot2::GeomSegment, stat = "identity", position = "identity",
  aes_form = NULL,
  extras = alist(
    model = , color = "#b599ed", label_color = "black", label_size = 3.5,
    arrow_linewidth = 0.5, show_b0 = TRUE, run = NULL, run_x = NULL,
    b0_alpha = 0.3, b0_linewidth = 0.8, b0_size = 4,
    arrow_nudge = 0.18, label_nudge = 0.08
  ),
  note = "the model whose coefficients to annotate: a fit from lm() or aov()",
  pre = {
    if (!missing(gformula) && missing(model)) {
      model <- gformula
      gformula <- NULL
    }

    if ((!missing(object) || !missing(model)) && !isTRUE(show.help)) {
      dots <- list(...)
      color <- dots$colour %||% color
      label_color <- dots$label_colour %||% label_color
      unreachable_dots <- dots[setdiff(names(dots), c("colour", "label_colour"))]
      args <- list(
        color = color, label_color = label_color, label_size = label_size,
        arrow_linewidth = arrow_linewidth, show_b0 = show_b0, run = run, run_x = run_x,
        b0_alpha = b0_alpha, b0_linewidth = b0_linewidth, b0_size = b0_size,
        arrow_nudge = arrow_nudge, label_nudge = label_nudge
      )
      coursekata:::gf_b_warn_unreachable(unreachable_dots, show.legend, "gf_coef")
      spec <- coursekata:::gf_b_spec(
        object, if (missing(model)) NULL else model, args, "gf_coef"
      )
      object <- spec$plot
      layer_fun <- coursekata:::gf_b_layer_fun(spec$marks)
    }
  }
)
