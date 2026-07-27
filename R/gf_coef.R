#' Annotate Model Coefficients on a Plot
#'
#' `r lifecycle::badge("experimental")`
#'
#' Overlays the coefficients of a linear model onto a `ggformula` plot so
#' students can see what each `b` in the model equation *is* on the graph. The
#' intercept `b0` is marked, and every other coefficient is drawn as an arrow
#' showing its size and direction:
#'
#' * **Categorical predictor** -- `b0` is a horizontal line at the reference
#'   group's mean, and each `b_k` is an arrow from that line to group `k`'s mean.
#' * **Continuous predictor** -- `b1` is drawn as a rise-over-run right triangle
#'   (the slope), and `b0` as a hollow dot where the line crosses `x = 0`.
#'
#' `gf_b()` is the name used most in teaching; `gf_coef()` is a fully supported
#' alias for it.
#'
#' @param p A ggformula plot object, typically from `gf_point()` or
#'   `gf_jitter()`.
#' @param model A fitted `lm()` object. If omitted, a model is fit as `y ~ x`
#'   from the plot's own mapping (locking any `shuffle()` first).
#' @param color Color for the arrows, lines, and dots (not the text labels).
#'   Default CourseKata purple `"#b599ed"`.
#' @param label_color Color for the text labels. Default `"black"`.
#' @param b0_color For continuous `x`, the color of the hollow `b0` dot.
#'   Defaults to `color`.
#' @param b0_alpha For categorical `x`, the transparency of the `b0` line.
#'   Default `0.3`.
#' @param b0_linewidth For categorical `x`, the width of the `b0` line. Default
#'   `0.8`.
#' @param b0_size For continuous `x`, the size of the hollow `b0` dot. Default
#'   `4`.
#' @param arrow_linewidth Width of the coefficient arrows. Default `0.5`.
#' @param label_size Font size for the coefficient labels. Default `3.5`.
#' @param show_b0_label Whether to annotate `b0`. Default `TRUE`.
#' @param arrow_nudge For categorical `x`, how far left of each group to place
#'   the arrow. Default `0.18`.
#' @param label_nudge For categorical `x`, extra leftward offset for the label.
#'   Default `0.08`.
#' @param run For continuous `x`, override the auto-selected run unit of the
#'   slope triangle.
#' @param run_x For continuous `x`, override the x position of the slope
#'   triangle.
#' @param ... Currently unused.
#'
#' @return A ggplot object with coefficient annotations added.
#'
#' @details
#' Best on a small sample so the arrows and labels do not crowd the points. For
#' the continuous case the "run" of the slope triangle is auto-selected as a
#' round number near 10% of the x span; pass `run` to set it yourself (e.g.
#' `run = 1` to show the slope per one-unit change).
#'
#' @export
#' @examples
#' # Continuous predictor: b1 is the slope, drawn as a rise-over-run triangle,
#' # and b0 is the hollow dot where the regression line meets x = 0.
#' model <- lm(Thumb ~ Height, data = Fingers)
#' gf_point(Thumb ~ Height, data = Fingers, alpha = .3) %>%
#'   gf_b(model)
#'
#' # Show the slope per one-unit change in Height by fixing the run to 1.
#' gf_point(Thumb ~ Height, data = Fingers, alpha = .3) %>%
#'   gf_b(model, run = 1)
#'
#' # Categorical predictor: b0 is the reference group's mean (the horizontal
#' # line), and each b_k is an arrow from b0 to that group's mean.
#' tip_model <- lm(Tip ~ Condition, data = TipExperiment)
#' gf_jitter(Tip ~ Condition, data = TipExperiment, width = .1) %>%
#'   gf_b(tip_model)
#'
#' # Omit the model and gf_b() fits y ~ x from the plot's own mapping.
#' gf_point(Thumb ~ Height, data = Fingers, alpha = .3) %>%
#'   gf_b()
#'
#' # gf_coef() is a fully supported alias with a longer, more formal name.
#' gf_point(Thumb ~ Height, data = Fingers, alpha = .3) %>%
#'   gf_coef(model)
gf_b <- function(p, model = NULL,
                    color = "#b599ed",
                    label_color = "black",
                    b0_color = NULL,
                    b0_alpha = 0.3,
                    b0_linewidth = 0.8,
                    b0_size = 4,
                    arrow_linewidth = 0.5,
                    label_size = 3.5,
                    show_b0_label = TRUE,
                    arrow_nudge = 0.18,
                    label_nudge = 0.08,
                    run = NULL,
                    run_x = NULL,
                    ...) {
  lifecycle::signal_stage("experimental", "gf_b()")

  if (is.null(b0_color)) b0_color <- color

  # If no model supplied, freeze the plot and fit one from the frozen data so
  # the arrows and the jitter dots reflect the same shuffle().
  if (is.null(model)) {
    p <- freeze_xy_values(p)
    model <- model_from_plot(p)
  }

  coefs <- stats::coef(model)
  b0_val <- coefs[[1]]

  # Detect categorical vs continuous from the model's data
  model_terms <- attr(stats::terms(model), "term.labels")
  is_cat <- if (length(model_terms) == 0) {
    FALSE
  } else {
    x_var <- model_terms[1]
    x_vals <- model$model[[x_var]]
    is.factor(x_vals) || is.character(x_vals)
  }

  # b0 horizontal line for categorical or intercept-only models; for continuous
  # x, b0 is shown as a hollow dot at x = 0 instead.
  out <- p
  if (is_cat || length(model_terms) == 0) {
    out <- out + ggplot2::geom_hline(
      yintercept = b0_val,
      color = color, alpha = b0_alpha, linewidth = b0_linewidth
    )
  }

  # Intercept-only model: just the line and optional label
  if (length(model_terms) == 0) {
    if (show_b0_label) {
      x_rng <- x_range_from_plot(p, model)
      out <- out + ggplot2::annotate(
        "label",
        x = x_rng[1], y = b0_val,
        label = "b[0]", parse = TRUE,
        color = label_color, size = label_size, hjust = 0, vjust = 0.5,
        fill = "white"
      )
    }
    return(out)
  }

  if (is_cat) {
    out <- add_cat_coefs(
      out, coefs, b0_val, is_cat, x_vals,
      color, label_color, arrow_linewidth, label_size,
      show_b0_label, arrow_nudge, label_nudge
    )
  } else {
    out <- add_cont_coef(
      out, coefs, b0_val, x_vals,
      color, label_color, b0_color, b0_size,
      arrow_linewidth, label_size,
      show_b0_label, run, run_x, p, model
    )
  }

  out
}

#' @rdname gf_b
#' @export
gf_coef <- gf_b


# Build an lm from the frozen plot data.
#
# @noRd
model_from_plot <- function(p) {
  p <- freeze_xy_values(p)
  y_vals <- p$data[[".gf_y"]]
  x_raw <- p$data[[".gf_x"]]

  if (!is.numeric(x_raw) && !is.factor(x_raw)) x_raw <- factor(x_raw)
  stats::lm(y ~ x, data = data.frame(y = y_vals, x = x_raw))
}

# Draw the b0 line label and one arrow per non-reference coefficient (cat x).
#
# @noRd
add_cat_coefs <- function(out, coefs, b0_val, is_cat, x_vals,
                          color, label_color, arrow_linewidth, label_size,
                          show_b0_label, arrow_nudge, label_nudge) {
  if (!is.factor(x_vals)) x_vals <- factor(x_vals)

  if (show_b0_label) {
    out <- out + ggplot2::annotate(
      "label",
      x = 1 - arrow_nudge - label_nudge, y = b0_val,
      label = "b[0]", parse = TRUE,
      color = label_color, size = label_size, hjust = 1, vjust = 0.5,
      fill = "white"
    )
  }

  if (length(coefs) < 2) {
    return(out)
  }

  for (i in seq(2, length(coefs))) {
    bi <- coefs[[i]]
    x_arr <- i - arrow_nudge
    x_lbl <- x_arr - label_nudge
    y_top <- b0_val + bi
    y_bot <- b0_val

    out <- out +
      ggplot2::geom_segment(
        data = data.frame(x = x_arr, xend = x_arr, y = y_top, yend = y_bot),
        ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
        inherit.aes = FALSE,
        color = color, linewidth = arrow_linewidth,
        arrow = ggplot2::arrow(
          ends = "first", type = "open",
          length = ggplot2::unit(0.15, "cm")
        )
      ) +
      ggplot2::annotate(
        "label",
        x = x_lbl, y = (y_top + y_bot) / 2,
        label = paste0("b[", i - 1, "]"), parse = TRUE,
        color = label_color, size = label_size, hjust = 1,
        fill = "white"
      )
  }

  out
}

# Draw the b1 rise-over-run triangle and the b0 hollow dot (continuous x).
#
# @noRd
add_cont_coef <- function(out, coefs, b0_val, x_vals,
                          color, label_color, b0_color, b0_size,
                          arrow_linewidth, label_size,
                          show_b0_label, run, run_x, p, model) {
  if (length(coefs) < 2) {
    return(out)
  }
  b1_val <- coefs[[2]]

  # Use the effective display range; when show_b0_label is TRUE the axis is
  # expanded to include 0, so compute the annotation from that wider range.
  x_data_range <- range(x_vals, na.rm = TRUE)
  x_display_range <- if (show_b0_label) range(c(x_data_range, 0)) else x_data_range
  x_span <- diff(x_display_range)
  x_lo <- x_display_range[1]

  if (is.null(run)) run <- nice_run(x_span)

  # Place the annotation away from x = 0 so it does not crowd the b0 dot.
  if (is.null(run_x)) {
    if (show_b0_label) {
      run_x_left <- x_lo + 0.15 * x_span
      run_x_right <- x_lo + 0.60 * x_span
      run_x <- if (abs(run_x_left + run / 2) >= abs(run_x_right + run / 2)) {
        run_x_left
      } else {
        run_x_right
      }
    } else {
      run_x <- x_lo + 0.15 * x_span
    }
  }

  y_base <- b0_val + b1_val * run_x
  y_tip <- b0_val + b1_val * (run_x + run)
  rise <- y_tip - y_base
  y_mid <- (y_base + y_tip) / 2

  # Vertical "rise" arrow
  out <- out + ggplot2::geom_segment(
    data = data.frame(x = run_x, xend = run_x, y = y_base, yend = y_tip),
    ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
    inherit.aes = FALSE,
    color = color, linewidth = arrow_linewidth,
    arrow = ggplot2::arrow(
      ends = "last", type = "open",
      length = ggplot2::unit(0.15, "cm")
    )
  )

  # Horizontal "run" segment at the tip of the arrow
  out <- out + ggplot2::geom_segment(
    data = data.frame(x = run_x, xend = run_x + run, y = y_tip, yend = y_tip),
    ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
    inherit.aes = FALSE,
    color = color, linewidth = arrow_linewidth
  )

  # Rise label: "b[1]" when run == 1, otherwise "run * b[1]"
  rise_label <- if (run == 1) "b[1]" else paste0(format_run(run), " %*% b[1]")
  out <- out + ggplot2::annotate(
    "label",
    x = run_x - 0.015 * x_span, y = y_mid,
    label = rise_label, parse = TRUE,
    color = label_color, size = label_size, hjust = 1,
    fill = "white"
  )

  # Run-distance label below (or above) the horizontal segment
  vjust_run <- if (rise >= 0) -0.4 else 1.4
  out <- out + ggplot2::annotate(
    "label",
    x = run_x + run / 2, y = y_tip,
    label = format_run(run),
    color = label_color, size = label_size - 0.5, vjust = vjust_run,
    fill = "white"
  )

  # b0: hollow dot at (0, b0), expanding the x scale so it is always visible
  if (show_b0_label) {
    out <- out +
      ggplot2::expand_limits(x = 0) +
      ggplot2::annotate(
        "point",
        x = 0, y = b0_val,
        color = b0_color, size = b0_size, shape = 1, stroke = 1.2
      ) +
      ggplot2::annotate(
        "label",
        x = 0, y = b0_val,
        label = "b[0]", parse = TRUE,
        color = label_color, size = label_size, hjust = -0.5, vjust = 0.5,
        fill = "white"
      )
  }

  out
}

# Pick a power of 10 in [5%, 80%] of the x span, closest to 10%. Falls back to
# a raw 10% of the span when no power of 10 qualifies.
#
# @noRd
nice_run <- function(x_span) {
  candidates <- 10^seq(-10, 10)
  ok <- candidates >= 0.05 * x_span & candidates <= 0.80 * x_span
  if (any(ok)) {
    valid <- candidates[ok]
    valid[which.min(abs(valid - 0.10 * x_span))]
  } else {
    0.10 * x_span
  }
}

# Format the run label cleanly (no trailing zeros, no scientific notation).
#
# @noRd
format_run <- function(run) {
  if (run == round(run)) {
    as.character(as.integer(run))
  } else {
    sub("\\.?0+$", "", sprintf("%.10g", run))
  }
}

# Get the x range from the plot data, falling back to the model data.
#
# @noRd
x_range_from_plot <- function(p, model) {
  d <- if (!is.null(p$data) && nrow(p$data) > 0) p$data else model$model
  x_col <- tryCatch(as_name(p$mapping$x), error = function(e) NULL)
  if (!is.null(x_col) && x_col %in% names(d)) {
    x_vals <- d[[x_col]]
    if (is.numeric(x_vals)) {
      return(range(x_vals, na.rm = TRUE))
    }
    return(c(1, length(unique(x_vals))))
  }
  c(0, 1)
}
