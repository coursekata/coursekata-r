#' Which axis a residual arrived measured on
#'
#' Exactly one of `xend`/`yend` is mapped -- the terminal companion of the axis
#' the plot put the model's outcome on -- so the column that is there is what
#' says which way the residual runs. Reading it back off the data is what keeps
#' every draw-time step (the segment, the square, the missing-value drop) in
#' agreement with that one call-time decision.
#'
#' @param data A layer's data.
#'
#' @return `"y"` or `"x"`.
#'
#' @noRd
resid_axis <- function(data) {
  if ("yend" %in% names(data)) "y" else "x"
}

#' Where a squared residual's four corners go
#'
#' The square is a square on the page, not in data units: the side drawn
#' across the residual is scaled by `aspect` and by the panel's range ratio.
#' The panel's ranges are only final at draw time, which is why this is called
#' from [GeomSquareResid]'s `draw_panel()` and not from a stat.
#'
#' @param data One row per observation, with `x`, `y` (the observation) and one
#'   of `yend`/`xend` (what the model predicts for it). Which one it is says
#'   which axis the residual runs along.
#' @param x_range,y_range The panel's ranges.
#' @param aspect Multiplier on the drawn side length.
#'
#' @return `data` with four rows per observation and one `group` per square.
#'
#' @noRd
square_vertices <- function(data, x_range, y_range, aspect) {
  # the residual runs along the axis the prediction arrived on; the square is
  # drawn out from it along the other one
  along <- resid_axis(data)
  across <- if (identical(along, "y")) "x" else "y"
  along_range <- if (identical(along, "y")) y_range else x_range
  across_range <- if (identical(along, "y")) x_range else y_range
  end <- data[[paste0(along, "end")]]

  ratio <- diff(across_range) / diff(along_range)
  side <- abs(data[[along]] - end) * aspect * ratio
  # squares extend away from the nearer edge so they stay inside the panel
  away <- ifelse(data[[across]] > mean(across_range), -1, 1)
  far <- data[[across]] + away * side

  vertices <- data[rep(seq_len(nrow(data)), each = 4), , drop = FALSE]
  vertices[[across]] <- as.vector(rbind(data[[across]], far, far, data[[across]]))
  vertices[[along]] <- as.vector(rbind(data[[along]], data[[along]], end, end))
  vertices$group <- rep(seq_len(nrow(data)), each = 4)
  rownames(vertices) <- NULL
  vertices
}

#' Carry a prediction alongside the observation it belongs to
#'
#' `xend`/`yend` is a positional aesthetic on purpose: that is what gets the
#' prediction transformed by its scale and trained into the panel's range.
#' Exactly one of the two arrives, on the axis the plot put the model's
#' outcome on.
#'
#' `compute_layer` is overridden to a pass-through, the same shape
#' [ggplot2::StatIdentity] uses, so an observation with an `NA` on it (a
#' predictor the model dropped) survives the stat instead of being removed
#' before the position runs. The residual's jitter is a function of the seed
#' and the rows it is handed, exactly as the point layer's is, so losing a row
#' here would hand it a different sequence of draws and the segment would land
#' away from its point.
#'
#' @format A [ggplot2::Stat] object.
#'
#' @seealso [gf_resid()] and [gf_square_resid()], which pair this stat and a
#'   geom for you.
#' @export
StatResid <- ggplot2::ggproto(
  "StatResid", ggplot2::Stat,
  required_aes = c("x", "y", "xend|yend"),
  compute_layer = function(self, data, params, layout) data,
  compute_panel = function(data, scales, ...) data
)

#' Jitter a residual's observed end the way its point was jittered
#'
#' A residual has to start where its point is drawn, and its point may have been
#' jittered. The grammar's answer to two layers agreeing is a seed: two layers
#' that each declare `position_jitter(width, height, seed)` land on identical
#' offsets without knowing about each other, because the offsets are a function
#' of the seed and the rows. So the residual declares its own jitter rather than
#' capturing the points layer's position and replaying it.
#'
#' It cannot be `position_jitter()` itself, because that transforms every
#' positional aesthetic and `yend` is a prediction: moving it would take the
#' fitted end off the model the residual is measured against. This applies the
#' same two draws, in the same order, to `x` and `y` alone -- which is what
#' makes the offsets identical to the points layer's rather than merely similar.
#'
#' Two things about how the points layer spends the stream are a CONTRACT here
#' rather than implementation details this file happens to share.
#'
#' The FIRST is where the seed is set. [ggplot2::PositionJitter] defines
#' `compute_panel`, not `compute_layer`, so ggplot2's own parent splits the
#' layer by panel and re-seeds inside each one. This does the same. Jittering
#' the whole layer in one sequence agrees with the points layer only while the
#' plot has one panel; facet it and every segment lands on some other
#' observation's offset.
#'
#' The SECOND is that `x` is drawn before `y`, here and in
#' [ggplot2::PositionJitter] itself. `jitter()` advances R's RNG stream by one draw per call, so
#' a residual layer that wants its `x` offsets to equal the points layer's has
#' to call `jitter(x, ...)` first and consume the same slice of the stream the
#' points layer consumed first. Drawing `y` unconditionally instead of guarding
#' it with `if (height > 0)`, or reordering the two draws, would silently move
#' the `x` offsets off the points layer's.
#'
#' A reduction's segments start at the grand mean, a single repeated number,
#' so there is nothing for the OUTCOME axis to gain from jittering -- but which
#' physical axis that is depends on the plot's orientation, and both draws
#' still have to happen, in the points layer's own order, for the OTHER axis's
#' offsets to match. `outcome` names the aesthetic (`"x"` or `"y"`) to hold
#' still: both draws run as usual and the held axis is restored to its
#' pre-jitter values afterward, rather than skipping one draw outright, so the
#' surviving axis's offsets are identical to the points layer's whichever axis
#' that is.
#'
#' @format A [ggplot2::Position] object.
#'
#' @seealso [gf_resid()], which pairs this position with the plot's own jitter.
#' @noRd
PositionResidJitter <- ggplot2::ggproto(
  "PositionResidJitter", ggplot2::Position,
  width = NULL, height = NULL, seed = NA, outcome = NULL,
  # mirrors position_jitter()'s own defaults, so an unspecified width or height
  # resolves to the same number for both layers
  setup_params = function(self, data) {
    list(
      width = self$width %||% (ggplot2::resolution(data$x, zero = FALSE) * 0.4),
      height = self$height %||% (ggplot2::resolution(data$y, zero = FALSE) * 0.4),
      seed = self$seed,
      outcome = self$outcome
    )
  },
  # WHICHEVER HOOK UPSTREAM USES, because the points layer's offsets are not
  # ours to choose and the two have to agree. On ggplot2 3.5.2
  # [ggplot2::PositionJitter] implements `compute_layer` and spends one
  # sequence over the whole layer; on 4.0 it implements `compute_panel`, so
  # ggplot2's own parent splits by panel and re-seeds inside each one. Pick the
  # wrong one and a faceted plot detaches every segment from its point --
  # measured on five panels, every segment displaced by up to the full jitter
  # width, while the unfaceted case stays exact either way.
  compute_layer = function(self, data, params, layout) {
    if (jitter_is_per_panel()) {
      # hand it back to Position, which splits by panel and calls the method below
      ggplot2::ggproto_parent(ggplot2::Position, self)$compute_layer(data, params, layout)
    } else {
      jitter_holding(data, params)
    }
  },
  compute_panel = function(data, params, scales) jitter_holding(data, params)
)

#' Jitter `x` and `y`, then put the outcome axis back where it was
#'
#' The body both of `PositionResidJitter`'s hooks share, so the offsets cannot
#' differ between the two paths -- see that object for why there are two.
#'
#' @param data A layer's data, whole or one panel's worth.
#' @param params The position's resolved params.
#'
#' @return `data`, with the jittered axes moved.
#'
#' @noRd
jitter_holding <- function(data, params) {
  with_fixed_seed(params$seed, {
    held <- if (!is.null(params$outcome)) data[[params$outcome]]
    if (params$width > 0) data$x <- jitter(data$x, amount = params$width)
    if (params$height > 0) data$y <- jitter(data$y, amount = params$height)
    if (!is.null(params$outcome)) data[[params$outcome]] <- held
    data
  })
}

#' @noRd
position_resid_jitter <- function(width = NULL, height = NULL, seed = NA, outcome = NULL) {
  ggplot2::ggproto(
    NULL, PositionResidJitter, width = width, height = height, seed = seed, outcome = outcome
  )
}

#' The position a residual has to be drawn with, and the plot to draw it on
#'
#' An unseeded jitter draws different offsets on every render, so nothing can
#' attach to it -- not by replay and not by declaration. The residual therefore
#' pins it, on the plot it returns rather than on the caller's: adding a
#' residual to a jittered plot gives back a plot whose jitter is fixed, which is
#' what makes the segment land on the point in the first place.
#'
#' @param plot The plot the residual is being drawn on.
#' @param outcome The aesthetic (`"x"` or `"y"`) to hold at its pre-jitter
#'   value, or `NULL` to jitter both -- see `PositionResidJitter`'s own docs
#'   for why holding rather than skipping a draw is what keeps the other
#'   axis's offsets identical to the points layer's.
#'
#' @return A list with `plot` and `position`.
#'
#' @noRd
resid_jitter <- function(plot, outcome = NULL) {
  plain <- list(plot = plot, position = "identity")
  if (length(plot$layers) == 0) {
    return(plain)
  }
  pos <- plot$layers[[1]]$position
  if (!inherits(pos, "PositionJitter")) {
    return(plain)
  }

  seed <- pos$seed
  if (!isTRUE(is.finite(seed))) {
    seed <- sample.int(.Machine$integer.max, 1L)
    plot$layers[[1]] <- layer_with_position(
      plot$layers[[1]],
      ggplot2::position_jitter(width = pos$width, height = pos$height, seed = seed)
    )
  }

  list(
    plot = plot,
    position = position_resid_jitter(pos$width, pos$height, seed, outcome = outcome)
  )
}

#' Draw a residual as a segment from a prediction to an observation
#'
#' The segment is drawn from what the model predicts to what was observed, so
#' `x`/`y` is the observation and `xend`/`yend` the prediction until the moment
#' of drawing. Which of the two ends arrives says which axis the residual is
#' measured on. Pair it with [StatResid] in a `ggplot2::layer()` to draw
#' residuals in a plot you are assembling yourself.
#'
#' @format A [ggplot2::Geom] object.
#'
#' @seealso [gf_resid()], which pairs this geom and [StatResid] for you.
#' @export
GeomResid <- ggplot2::ggproto(
  "GeomResid", ggplot2::GeomSegment,
  required_aes = c("x", "y", "xend|yend"),
  handle_na = function(self, data, params) {
    # the inherited drop reads `required_aes` literally, and `"xend|yend"` is
    # not a column, so neither end would ever be checked: name the one that
    # arrived
    ggplot2::remove_missing(
      data, params$na.rm,
      c("x", "y", paste0(resid_axis(data), "end"), self$non_missing_aes),
      "geom_resid"
    )
  },
  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        arrow.fill = NULL, lineend = "butt",
                        linejoin = "round", na.rm = FALSE) {
    segments <- if (identical(resid_axis(data), "y")) {
      transform(data, xend = x, y = yend, yend = y)
    } else {
      transform(data, yend = y, x = xend, xend = x)
    }
    ggplot2::GeomSegment$draw_panel(
      segments, panel_params, coord,
      arrow = arrow, arrow.fill = arrow.fill,
      lineend = lineend, linejoin = linejoin, na.rm = na.rm
    )
  }
)

#' Draw a residual as the square it would make
#'
#' The square is expanded here rather than in [StatResid] because a stat runs
#' before the position: four corners jittered one row at a time tear apart.
#' It is also the only place the panel's final ranges are known, and the
#' square is a square on the page rather than in data units.
#'
#' @format A [ggplot2::Geom] object.
#'
#' @seealso [gf_square_resid()], which pairs this geom and [StatResid] for you.
#' @export
GeomSquareResid <- ggplot2::ggproto(
  "GeomSquareResid", ggplot2::GeomPolygon,
  required_aes = c("x", "y", "xend|yend"),
  draw_panel = function(self, data, panel_params, coord, aspect = 4 / 6,
                        rule = "evenodd", lineend = "butt", linejoin = "round",
                        linemitre = 10, na.rm = FALSE) {
    keep <- c("x", "y", paste0(resid_axis(data), "end"))
    data <- data[stats::complete.cases(data[keep]), , drop = FALSE]
    if (nrow(data) == 0) {
      return(ggplot2::zeroGrob())
    }
    ggplot2::GeomPolygon$draw_panel(
      square_vertices(data, panel_params$x.range, panel_params$y.range, aspect),
      panel_params, coord,
      rule = rule, lineend = lineend, linejoin = linejoin, linemitre = linemitre
    )
  }
)


#' What the model predicts for every row the plot holds
#'
#' Predicting over the plot's own data, rather than reading the model's fitted
#' values, is what keeps the two aligned when `lm()` has dropped rows: the
#' rows it dropped for missingness are the rows the plot does not draw either.
#'
#' @noRd
resid_fitted <- function(model, data, call = caller_env()) {
  tryCatch(
    stats::predict(model, newdata = data),
    error = function(cnd) {
      used <- all.vars(stats::formula(model)[-2])
      absent <- setdiff(used, names(data))
      if (length(absent) == 0) {
        abort(conditionMessage(cnd), parent = cnd, call = call)
      }
      abort(
        c(
          "The model uses variables the plot's data does not have",
          glue("model: {collapse(used)}"),
          glue("missing from the plot's data: {collapse(absent)}")
        ),
        parent = cnd,
        call = call
      )
    }
  )
}

#' Which end aesthetic a model's residuals are measured on
#'
#' The plot has already put the model's outcome on an axis; the residual is the
#' distance along that axis, so the prediction is named on that axis's terminal
#' companion. If the outcome is on neither axis there is no such distance: the
#' segment would run from an observation of one variable to a prediction of
#' another, which is a plausible picture of nothing. `gf_model()` refuses that
#' pairing and so does this.
#'
#' @noRd
resid_end <- function(spec, model, call = caller_env()) {
  mspec <- model_spec(spec$data, model, call = call)
  outcome_axis <- names(spec$axes[spec$axes %in% mspec$outcome])
  if (length(outcome_axis) == 0) {
    axes <- purrr::imap_chr(spec$axes, function(variable, aes) glue("{aes} = {variable}"))
    abort(
      c(
        "A residual is measured along the axis carrying the model's outcome",
        glue("the model predicts: {collapse(mspec$outcome)}"),
        glue("the plot's axes are: {collapse(axes)}"),
        "plot the outcome this model predicts, or measure the model this plot was built for"
      ),
      call = call
    )
  }
  if (identical(outcome_axis, "x")) "xend" else "yend"
}

#' @param from `NULL` for today's behavior: both axes are the plot's own
#'   quosures. A column name to replace the positional aesthetic on the
#'   outcome's axis with a `.data`-pronoun read of that column instead --
#'   what `reduce_spec()` uses to start a reduction's segments at `.grand`
#'   rather than at the plot's own observed values, while the other axis
#'   still reads what the plot reads.
#'
#' @noRd
resid_mapping <- function(spec, end, from = NULL) {
  mapping <- ggplot2::aes(yend = .data$.fitted)
  names(mapping) <- end
  # state the axes rather than inherit them: a plot built with ggplot2 directly
  # carries them on its first layer, where there is nothing to inherit from,
  # and the squares take the geom's own fill and colour so they cannot inherit
  # at all. spec$mapping is the pinned quosures -- this draws what the plot
  # draws, so leave it be; anything read here has to evaluate, not print.
  mapping[c("x", "y")] <- spec$mapping[c("x", "y")]
  if (!is.null(from)) {
    # the outcome's axis is the one `end` names the terminal companion of
    # ("yend" -> "y", "xend" -> "x"); the other axis is left alone
    axis <- sub("end$", "", end)
    mapping[[axis]] <- rlang::quo(.data[[from]])
  }
  mapping
}

#' Refuse a fit whose squares would not add up
#'
#' The whole claim of a reduction is an identity: the error the empty model
#' leaves, the error this model leaves, and the distance between the two
#' predictions are three areas that add up. A reader is being shown PRE as a
#' ratio of areas they can count, so an area that does not belong to the
#' identity is not a rough picture of the right idea -- it is a picture that
#' teaches an arithmetic that does not hold.
#'
#' The identity needs `sum((y - fitted) * (fitted - grand))` to vanish, which
#' ordinary least squares gives for free BECAUSE it fits an intercept: the
#' residuals come out orthogonal to a constant, so they are orthogonal to the
#' grand mean. Two fits break it. Without an intercept there is no such
#' guarantee -- measured on `lm(Thumb ~ Height - 1, data = Fingers)`, the two
#' parts come to 11700.01 against a total of 11880.21, so about 180 of the
#' total belongs to neither square. With weights the residuals are orthogonal
#' in the weighted inner product instead, while the squares on the page are
#' plain areas; measured, the parts overshoot the total by about 11.
#'
#' A refusal rather than a warning, and rather than a weighted baseline: this
#' package's reductions are drawn to be counted, and there is no reading of a
#' drawn square that is right for a fit whose own arithmetic is different.
#'
#' @param model A model fit by `lm()` or `aov()`.
#' @param fn The name to refuse in, e.g. `"gf_reduce"`.
#' @param call The calling environment, for error reporting.
#'
#' @return `model`, invisibly.
#'
#' @noRd
check_decomposable <- function(model, fn, call = caller_env()) {
  no_intercept <- identical(as.integer(attr(stats::terms(model), "intercept")), 0L)
  weighted <- length(model$weights) > 0

  if (!no_intercept && !weighted) {
    return(invisible(model))
  }

  abort(
    c(
      glue("`{fn}()` draws a reduction that this model's own arithmetic does not support"),
      x = if (no_intercept) {
        "this model was fit without an intercept"
      } else {
        "this model was fit with weights"
      },
      "*" = paste(
        "total, error and reduction only add up to each other when a model's residuals",
        "are orthogonal to the grand mean, and it is the intercept that guarantees that"
      ),
      i = paste(
        if (no_intercept) "fit the model with its intercept" else "fit the model unweighted",
        "or measure it with `gf_resid()`, which needs no such identity"
      )
    ),
    call = call
  )
}

#' Refuse a plot that does not draw both of the axes a residual spans
#'
#' Lives apart from the two spec functions because both need it and both have to
#' fire it at the same moment: after the plot has been read, and before anything
#' is predicted, so that a plot with no y is reported as a plot rather than as a
#' model that could not be predicted or a function that could not be called. One
#' guard, one message, two callers -- `resid_spec()` and `resid_fun_spec()`.
#' `call` is threaded through so the refusal names the function the reader wrote
#' rather than the helper it landed in.
#'
#' @param spec A `plot_spec()`.
#' @param call The calling environment, for error reporting.
#'
#' @return `spec`, invisibly.
#'
#' @noRd
check_resid_axes <- function(spec, call = caller_env()) {
  absent <- c("x", "y")[purrr::map_lgl(c("x", "y"), ~ is.null(spec$mapping[[.x]]))]
  if (length(absent) > 0) {
    # the reader's own spelling -- spec$mapping's quosures are pinned, and
    # printing `.coursekata_pin_y` at a reader is exactly the refusal this
    # guards against
    mapped <- purrr::imap_chr(spec$labels, function(label, aes) glue("{aes} = {label}"))
    abort(
      c(
        "A residual needs both an x and a y on the plot",
        glue("the plot maps: {if (length(mapped) > 0) collapse(mapped) else 'nothing'}"),
        glue("missing: {collapse(absent)}")
      ),
      call = call
    )
  }
  invisible(spec)
}

#' Refuse anything but a plot to layer a residual onto
#'
#' The first refusal both spec functions make, and the only line of the two that
#' is word for word the same, so it lives here rather than twice. `fn` is the
#' name the caller wrote: a helper naming itself would name a function the reader
#' never called.
#'
#' @param object The plot the layer is being added to.
#' @param fn The name to refuse in, e.g. `"gf_resid"`.
#' @param call The calling environment, for error reporting.
#'
#' @return `object`, invisibly.
#'
#' @noRd
check_resid_plot <- function(object, fn, call = caller_env()) {
  if (!inherits(object, c("gg", "ggplot"))) {
    abort(glue("`{fn}()` needs to be layered on top of a plot."), call = call)
  }
  invisible(object)
}

#' The two things a residual layer is built from, once its prediction is known
#'
#' The tail both spec functions share: the plot's own whole data frame with the
#' prediction carried alongside it as `.fitted`, and the mapping that states the
#' axes and names the end aesthetic. Keeping the plot's whole data frame is what
#' lets facets partition it and lets ggplot2 drop missing rows from the overlay
#' exactly as it does from the points.
#'
#' It is handed `end` rather than deriving it, because deriving it is precisely
#' what the two callers do differently.
#'
#' @param spec A `plot_spec()`.
#' @param fitted One prediction per row of `spec$data`.
#' @param end `"xend"` or `"yend"`: the axis the residual is measured along.
#'
#' @return A list with `data` (the plot's own data plus `.fitted`) and
#'   `aesthetics`.
#'
#' @noRd
resid_pieces <- function(spec, fitted, end) {
  data <- spec$data
  data$.fitted <- fitted
  list(data = data, aesthetics = resid_mapping(spec, end))
}

#' Translate a plot and a model into the pieces a residual layer is built from
#'
#' The counterpart of `model_layer_spec()`: everything `layer_factory()`'s `pre`
#' has to shadow, decided in a function rather than in a quoted block. It
#' returns only `data` and `aesthetics` because which geom draws the residual,
#' whether it inherits, and what it is tagged are per-function facts the factory
#' call already states -- so all three model entry points call this and state
#' their own.
#'
#' The refusals fire in the order the bespoke helper's lazy arguments forced
#' them, and that order is what the recorded messages depend on: not a
#' plot, then no model, then no x/y on the plot, then the prediction, then the
#' axis the outcome is on. A histogram of `Thumb` measured against
#' `Thumb ~ Height` has to say "needs both an x and a y", not "the axis carrying
#' the model's outcome", so the axis guard has to precede `resid_end()`.
#'
#' @param object The plot the layer is being added to.
#' @param model A model fit by `lm()` or `aov()`.
#' @param fn The name to refuse in, e.g. `"gf_resid"`.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with `data` (the plot's own data plus `.fitted`) and
#'   `aesthetics`.
#'
#' @noRd
resid_spec <- function(object, model, fn = "gf_resid", call = caller_env()) {
  check_resid_plot(object, fn, call = call)
  if (is.null(model)) {
    abort(
      c(
        glue("`{fn}()` needs to be told which model to measure residuals from"),
        i = glue("a model you already fit: `{fn}(lm(Thumb ~ Height, data = Fingers))`")
      ),
      call = call
    )
  }
  spec <- plot_spec(object)
  check_resid_axes(spec, call = call)
  fitted <- resid_fitted(model, spec$data, call = call)
  resid_pieces(spec, fitted, resid_end(spec, model, call = call))
}

#' Translate a plot and a function of x into the pieces a residual layer is built from
#'
#' The sibling of `resid_spec()`, and a sibling rather than a mode of it, because
#' the two compute different things. A model is asked what it predicts through
#' `stats::predict()`, and which axis its residuals run along has to be
#' discovered: `resid_end()` looks for the axis the plot put the outcome on and
#' refuses a model whose outcome the plot does not draw. A function of x predicts
#' y by definition -- there is no outcome to look for, `resid_end()` never runs,
#' and the end aesthetic is stated here as `"yend"`. Folding the two together
#' would put a branch on the very thing the fold claims to unify, and it would
#' cost the refusal its wording: this argument has to be named `fun`, because R
#' resolves the call `fun(...)` by searching the lexical chain for a function of
#' that name, and `could not find function "fun"` is what a caller who writes a
#' model or a formula here has always been told.
#'
#' What the two do share is everything around that difference, and they share it
#' through `check_resid_plot()`, `check_resid_axes()` and `resid_pieces()` rather
#' than by copy: the price of two entry points is that they must not drift, and
#' the drift has nowhere to happen if the common half is only written once.
#'
#' The refusals fire in the same order `resid_spec()`'s do: not a plot, then
#' nothing to measure, then no x/y on the plot, then the prediction. The axis
#' guard has to precede the call to `fun`, so that a histogram measured against a
#' function that cannot be called reports the plot rather than the function.
#'
#' @param object The plot the layer is being added to.
#' @param fun A function of the plot's x values returning a predicted y for each.
#' @param fn The name to refuse in, e.g. `"gf_resid_fun"`.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with `data` (the plot's own data plus `.fitted`) and
#'   `aesthetics`.
#'
#' @noRd
resid_fun_spec <- function(object, fun, fn = "gf_resid_fun", call = caller_env()) {
  check_resid_plot(object, fn, call = call)
  if (is.null(fun)) {
    abort(
      c(
        glue("`{fn}()` needs to be told which function to measure residuals from"),
        i = glue("a function of x: `{fn}(function(x) 2 + 3 * x)`")
      ),
      call = call
    )
  }
  spec <- plot_spec(object)
  check_resid_axes(spec, call = call)
  # `plot_x_values()` is the whole of "a function of x": the x quosure evaluated
  # in the plot's own data, which is what a caller who wrote `gf_function(f)`
  # above this line already drew `f` over
  fitted <- fun(plot_x_values(spec))
  # stated, not derived: a function of x predicts y, so there is no outcome to
  # look for and `resid_end()` never runs here
  resid_pieces(spec, fitted, "yend")
}

#' Translate a plot and a model into the pieces a reduction layer is built from
#'
#' A SIBLING of `resid_spec()`, not a mode of it, because the two measure
#' different distances. A residual runs from an observation to a prediction; a
#' reduction runs from one model's prediction to another's -- the empty
#' model's, always -- and the observation itself never enters the picture
#' except to say where along the other axis the segment sits. That is why the
#' segment's start is `.grand`, a single number repeated down every row,
#' rather than anything read off `spec$data`.
#'
#' It shares `check_resid_plot()`, `plot_spec()`, `check_resid_axes()`,
#' `resid_fitted()` and `resid_end()` with `resid_spec()`, called in that exact
#' order, because the refusals a reader sees have to fire in the same sequence
#' whichever of the two functions found the problem: not a plot, then no model,
#' then no x/y on the plot, then the prediction, then the axis the outcome is
#' on.
#'
#' The grand mean is the mean of `model`'s own model frame's first column --
#' the outcome the model itself was fit on -- rather than `mean(fitted(model))`.
#' The two agree
#' for any model with an intercept, which is every model this package ever
#' hands back, but only one of them is still the empty model's prediction if
#' that ever changes, and only one of them is correct for a model fit on data
#' narrower than the plot's own (a dropped-row model, or a model fit on a
#' sample the plot was not built from).
#'
#' @param object The plot the layer is being added to.
#' @param model A model fit by `lm()` or `aov()`.
#' @param fn The name to refuse in, e.g. `"gf_reduce"`.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with `data` (the plot's own data plus `.fitted` and
#'   `.grand`) and `aesthetics`.
#'
#' @noRd
reduce_spec <- function(object, model, fn = "gf_reduce", call = caller_env()) {
  check_resid_plot(object, fn, call = call)
  if (is.null(model)) {
    abort(
      c(
        glue("`{fn}()` needs to be told which model to measure the reduction of"),
        i = glue("a model you already fit: `{fn}(lm(Thumb ~ Height, data = Fingers))`")
      ),
      call = call
    )
  }
  spec <- plot_spec(object)
  check_resid_axes(spec, call = call)
  fitted <- resid_fitted(model, spec$data, call = call)
  end <- resid_end(spec, model, call = call)

  check_decomposable(model, fn, call = call)

  # the empty model has no predictors, so model$model (the frame it was fit
  # on) holds only the outcome column
  if (ncol(model$model) <= 1) {
    warn(
      c(
        glue("`{fn}()` was given the empty model, so every reduction is zero"),
        "*" = "the empty model IS the grand mean; there is nothing for it to reduce",
        "*" = "pass the model whose predictor you want to see the work of"
      ),
      class = "coursekata_reduce_empty"
    )
  }

  # the empty model's prediction: the mean of the outcome the model was fit on,
  # not `mean(fitted(model))` -- see the note above
  grand <- mean(model$model[[1]])
  data <- spec$data
  data$.fitted <- fitted
  data$.grand <- grand
  list(data = data, aesthetics = resid_mapping(spec, end, from = ".grand"))
}

#' Build the layer that draws residuals, the way `layer_factory()` asks for one
#'
#' Three things `ggplot2::layer` alone cannot do here.
#'
#' It cannot tag the layer, and `layer_index()` is how every test and every
#' example finds the residual.
#'
#' It would let `model` or `fun` through. An extra reaches here in `params`
#' whenever the caller wrote `model =` or `fun =`; given positionally either one
#' arrives as `gformula` instead, which ggformula drops on its own. Measured:
#' `gf_resid_fun(p, fun = f)` hands this function `params = fun, linewidth` where
#' `gf_resid_fun(p, f)` hands it `params = linewidth`. Both name what to draw
#' rather than how, so both are removed, and that is what makes the two spellings
#' build the identical layer. Measured honestly the other way too: at ggplot2
#' 4.0.3 `layer()` under `check.param = FALSE` already discards a parameter no
#' geom or stat claims, so neither line changes the built layer there. They stay
#' because the two spellings genuinely arrive carrying different `params`,
#' because this package supports ggplot2 back to 3.5.2, and because a rule stated
#' for `model` and quietly not applied to `fun` is the drift this file exists to
#' prevent. Unlike `model_layer_fun()`, everything else the caller wrote is kept:
#' `linewidth`, `aspect`, `color`, `alpha` and `linetype` are the whole point of
#' `...` here and there is no plan to supply them instead.
#'
#' And it would build the mapping in the wrong environment. `gf_ingredients()`
#' rewrites every quosure's environment to the caller's frame; the x and y
#' quosures came from the plot, where the expression may name something only the
#' plot's own frame has. Putting the plot's quosures back over the copies is
#' what keeps such a plot measurable; anything mapped on this call keeps the
#' caller's frame, which is where it was written.
#'
#' What it deliberately does NOT do is second-guess `check.param`. The two
#' families this package extends disagree about a misspelled parameter --
#' `ggplot2::layer()` defaults to `TRUE` and warns, `layer_factory()` always
#' passes `FALSE` and discards it in silence -- and each is right within its own
#' idiom. `gf_resid()` is a `gf_` function, so it takes ggformula's answer and is
#' silent, the same as `gf_point()` beneath it in the pipe and the same as
#' `gf_model()` beside it; a reader who misspells `color` in a pipe should not
#' get a warning on one line and silence on the next. Anyone assembling the
#' layer the ggplot2 way, with `layer(geom = GeomResid, stat = StatResid, ...)`,
#' gets ggplot2's warning already, for free, because nothing here is in the way.
#' Passing `check.param` through is what keeps both true at once.
#'
#' @param tag The tag to name the layer with.
#' @param aesthetics The mapping `resid_spec()` or `resid_fun_spec()` computed,
#'   with its own quosures.
#'
#' @return A function with the formals `layer_factory()` expects. It must name
#'   `geom`, `stat`, `position` and `params`: a `...`-only shim is stripped of
#'   all three by `create_formals()` and fails with a missing geom.
#'
#' @noRd
resid_layer_fun <- function(tag, aesthetics) {
  force(tag)
  force(aesthetics)
  function(geom, stat, position, params = NULL, mapping = NULL, data = NULL,
           check.param = FALSE, ...) {
    params[["model"]] <- NULL
    params[["fun"]] <- NULL
    mapping[names(aesthetics)] <- aesthetics
    tag_layer(
      ggplot2::layer(
        geom = geom, stat = stat, position = position,
        mapping = mapping, data = data, params = params,
        check.param = check.param, ...
      ),
      tag
    )
  }
}

#' The x values the plot draws, as the caller's function will be handed them
#'
#' The x quosure evaluated in the plot's own data, so a plotted expression
#' (`~log(Height)`) is measured as the expression and a discrete x arrives as the
#' factor it is drawn as. `resid_fun_spec()` is its only caller. `spec$mapping`
#' is the pinned quosure -- correct as written, this has to evaluate to the
#' values the plot draws, not to what the reader wrote.
#'
#' @noRd
plot_x_values <- function(spec) eval_tidy(spec$mapping$x, spec$data)
