#' Resolve gf_squareplot()'s formula-or-vector input to a numeric vector
#'
#' @param x A one-sided formula naming a variable, or a numeric vector.
#' @param data A data frame, required when `x` is a formula naming a column.
#' @param na.rm Must be `TRUE`; `FALSE` is refused because a missing value has
#'   no rectangle to draw.
#' @param env Where to look for the variable when `data` is not supplied.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with `values`, `label`, `is_factor` and `levels`.
#'
#' @noRd
squareplot_values <- function(x, data = NULL, na.rm = TRUE, env = caller_env(),
                              call = caller_env()) {
  if (!na.rm) {
    abort(
      c(
        "`na.rm = FALSE` is not supported",
        "gf_squareplot() draws one rectangle per observation",
        "a missing value has no rectangle to draw",
        "drop the missing values before plotting, or leave `na.rm = TRUE`"
      ),
      call = call
    )
  }

  if (inherits(x, "formula")) {
    if (!is.null(f_lhs(x))) {
      abort(
        c(
          "`x` must be a one-sided formula naming a single variable",
          glue("found: {deparse(x)}"),
          "drop the left-hand side, or pass the variable directly as ~var"
        ),
        call = call
      )
    }
    expr <- f_rhs(x)
    if (!is_symbol(expr)) {
      abort(
        c(
          "`x` must name a single variable, not an expression",
          glue("found: {deparse(expr)}"),
          "compute the variable first, then plot it"
        ),
        call = call
      )
    }
    name <- as_name(expr)
    if (is.null(data)) {
      raw <- tryCatch(
        get(name, envir = env),
        error = function(e) {
          abort(glue("Can't find `{name}`; supply `data` or define it first"), call = call)
        }
      )
    } else {
      if (name %in% names(data) == FALSE) {
        abort(
          c(
            glue("Can't find `{name}` in `data`"),
            glue("available: {collapse(names(data))}")
          ),
          call = call
        )
      }
      raw <- data[[name]]
    }
    label <- name
  } else {
    raw <- x
    label <- NULL
  }

  is_factor <- is.factor(raw)
  levels_num <- NULL
  if (is_factor) {
    levels_num <- suppressWarnings(as.numeric(levels(raw)))
    if (anyNA(levels_num)) {
      abort(
        c(
          glue("`{label %||% 'x'}` is a factor whose levels are not numbers"),
          glue("levels: {collapse(levels(raw))}")
        ),
        call = call
      )
    }
    values <- as.numeric(as.character(raw))
  } else {
    values <- raw
  }

  values <- values[!is.na(values)]
  if (!is.numeric(values)) {
    abort(
      c(
        glue("`{label %||% 'x'}` must be numeric"),
        glue("it is {class(raw)[[1]]}")
      ),
      call = call
    )
  }
  if (length(values) == 0) {
    abort(glue("`{label %||% 'x'}` has no non-missing values"), call = call)
  }

  list(values = values, label = label, is_factor = is_factor, levels = levels_num)
}

#' Countable-Rectangle Histogram
#'
#' `r lifecycle::badge("experimental")`
#'
#' Creates histograms where each observation is drawn as its own square, stacked
#' into columns, so a bin's height can be counted as well as read off the axis:
#' `n = 47` is 47 squares. Designed for teaching statistical concepts like
#' sampling distributions and hypothesis testing.
#'
#' @details
#' Sensible defaults are chosen based on the data:
#'
#' - For integer-valued data with a small range, the `binwidth` defaults to 1
#'   so that each integer gets its own column.
#' - When the input is a factor with numeric levels and `bars` is `"outline"` or
#'   `"solid"`, all levels are displayed on the x-axis even if some have zero
#'   counts.
#' - The white separator between two squares is capped at a quarter of a square's
#'   smaller side, so as a bin fills and its squares shrink the separator thins
#'   with them and the squares stay countable.
#'
#' When teaching about hypothesis testing, the `show_dgp = TRUE` overlay
#' frames a sampling distribution with its data generating process. It shows a
#' top axis labeled "Population Parameter (DGP)" with the population model
#' equation, a bottom axis labeled "Parameter Estimate" with the sample
#' estimate equation, and a red triangle marking the null hypothesis position
#' (\eqn{\beta_1 = 0}).
#'
#' @param x Formula (`~variable`) or numeric vector.
#' @param data Data frame (required if `x` is a formula).
#' @param binwidth Width of histogram bins. Auto-calculated if `NULL`.
#' @param origin Starting position for bins.
#' @param boundary Alias for `origin`.
#' @param fill Rectangle fill color. Default `"#7fcecc"`.
#' @param color Outline colour for the bars drawn when `bars` is `"outline"` or
#'   `"solid"`; the separators between squares are always white. `"black"` is
#'   drawn as `grey20`.
#' @param alpha Transparency. Default `1`.
#' @param na.rm Must be `TRUE`. Missing values have no rectangle to draw, so they
#'   are always dropped.
#' @param mincount Minimum y-axis height for consistent scaling.
#' @param bars Display style: `"none"` (squares only), `"outline"`, or
#'   `"solid"`.
#' @param xbreaks Number of x-axis breaks or vector of specific positions.
#' @param xrange X-axis limits as `c(min, max)`.
#' @param show_dgp Show DGP annotation overlay. Default `FALSE`.
#' @param show_mean Show dashed mean line. Default `FALSE`.
#' @param auto_subdivide Accepted but ignored.
#'
#' @return A ggplot object with S3 class `c("gf_squareplot", "gg", "ggplot")`.
#'
#' @seealso
#' The sampling distributions guide shows this plot in the context of a full
#' shuffle-and-estimate workflow:
#' <https://coursekata.github.io/coursekata-r/articles/sampling-distributions.html>
#'
#' @export
#' @examples
#' # each observation is a countable square
#' gf_squareplot(~Thumb, data = Fingers)
#'
#' # `bars` controls the display: "none" (default), "outline", or "solid"
#' gf_squareplot(~Thumb, data = Fingers, bars = "outline")
#'
#' # customize fill color, binwidth, and axis limits
#' gf_squareplot(~Thumb,
#'   data = Fingers,
#'   fill = "coral",
#'   binwidth = 5,
#'   xrange = c(30, 90)
#' )
#'
#' # integer data with a small range gets one column per integer
#' int_data <- data.frame(rolls = sample(1:6, 30, replace = TRUE))
#' gf_squareplot(~rolls, data = int_data)
#'
#' # with 2000 observations the squares shrink, and their separators thin to fit
#' set.seed(24)
#' large_data <- data.frame(x = rnorm(2000, mean = 50, sd = 10))
#' gf_squareplot(~x, data = large_data)
#'
#' # show a dashed line at the sample mean
#' gf_squareplot(~Thumb, data = Fingers, show_mean = TRUE)
#'
#' # frame a sampling distribution with its data generating process: with only
#' # 10 shuffles, the mean of the distribution (dashed red line) can land far
#' # from the null hypothesis marker on the top axis
#' shuffled_b1 <- function(n) {
#'   data.frame(b1 = replicate(n, {
#'     shuffled_tip <- base::sample(TipExperiment$Tip)
#'     b1(lm(shuffled_tip ~ Condition, data = TipExperiment))
#'   }))
#' }
#'
#' set.seed(42)
#' gf_squareplot(~b1,
#'   data = shuffled_b1(10),
#'   show_dgp = TRUE,
#'   show_mean = TRUE,
#'   xrange = c(-30, 30),
#'   mincount = 10,
#'   binwidth = 2
#' )
#'
#' # with 100 shuffles the mean moves close to the null; `mincount` keeps the
#' # y-axis fixed so the two plots are directly comparable
#' set.seed(42)
#' gf_squareplot(~b1,
#'   data = shuffled_b1(100),
#'   show_dgp = TRUE,
#'   show_mean = TRUE,
#'   xrange = c(-30, 30),
#'   mincount = 10,
#'   binwidth = 2
#' )
#'
#' # a factor with numeric levels gets one column per level
#' ratings <- data.frame(rating = factor(
#'   base::sample(1:5, 20, replace = TRUE, prob = c(1, 2, 4, 2, 1)),
#'   levels = 1:5
#' ))
#' gf_squareplot(~rating, data = ratings)
gf_squareplot <- function(x,
                          data = NULL,
                          binwidth = NULL,
                          origin = NULL,
                          boundary = NULL,
                          fill = "#7fcecc",
                          color = "black",
                          alpha = 1,
                          na.rm = TRUE,
                          mincount = NULL,
                          bars = c("none", "outline", "solid"),
                          xbreaks = NULL,
                          xrange = NULL,
                          show_dgp = FALSE,
                          show_mean = FALSE,
                          auto_subdivide = FALSE) {
  lifecycle::signal_stage("experimental", "gf_squareplot()")
  bars <- match.arg(bars)
  dgp_color <- "#003d70"

  input <- squareplot_values(x, data, na.rm, env = parent.frame())
  x_vec <- input$values
  x_label <- input$label
  is_factor <- input$is_factor
  factor_levels <- if (is_factor) as.character(input$levels) else NULL

  # --- binwidth ------------------------------------------------------------
  # a factor's levels, not its observed values, set the grid: one column per level
  if (is.null(binwidth)) binwidth <- if (is_factor) 1 else squareplot_binwidth(x_vec)

  # --- origin / boundary ---------------------------------------------------
  if (!is.null(boundary)) {
    origin <- boundary
  } else if (is.null(origin)) {
    if (is_factor) {
      origin <- min(as.numeric(factor_levels))
    } else {
      origin <- squareplot_origin(x_vec, binwidth)
    }
  }

  # --- assign bins ---------------------------------------------------------
  bin <- floor((x_vec - origin) / binwidth)
  counts_per_bin <- table(bin)

  # --- bar counts ----------------------------------------------------------
  if (length(counts_per_bin) > 0) {
    filled_bins <- as.numeric(names(counts_per_bin))
    bar_df <- data.frame(
      xmin = origin + filled_bins * binwidth,
      xmax = origin + (filled_bins + 1) * binwidth,
      count = as.numeric(counts_per_bin)
    )
    max_count <- max(bar_df$count)
  } else {
    bar_df <- data.frame(xmin = numeric(0), xmax = numeric(0),
                         count = numeric(0))
    max_count <- 0
  }

  # For factors, ensure all levels represented
  if (is_factor && binwidth == 1) {
    factor_levels_num <- as.numeric(factor_levels)
    all_bins <- floor((factor_levels_num - origin) / binwidth)
    complete_bar_df <- data.frame(
      bin = all_bins,
      xmin = origin + all_bins * binwidth,
      xmax = origin + (all_bins + 1) * binwidth,
      count = 0
    )
    if (nrow(bar_df) > 0) {
      for (i in seq_len(nrow(bar_df))) {
        idx <- which(complete_bar_df$xmin == bar_df$xmin[i])
        if (length(idx) > 0) {
          complete_bar_df$count[idx] <- bar_df$count[i]
        }
      }
    }
    bar_df <- complete_bar_df[, c("xmin", "xmax", "count")]
    if (nrow(bar_df) > 0) max_count <- max(max_count, max(bar_df$count))
  }

  max_plot_count <- max(max_count, mincount %||% max_count)
  extra_top <- if (show_dgp) max(3, 0.25 * max_plot_count) else 0
  y_upper <- max_plot_count + extra_top + 1.0

  # --- y-axis ticks --------------------------------------------------------
  if (max_plot_count <= 10) {
    step_y <- 1
  } else if (max_plot_count <= 20) {
    step_y <- 2
  } else if (max_plot_count <= 50) {
    step_y <- 5
  } else if (max_plot_count <= 100) {
    step_y <- 10
  } else {
    step_y <- ceiling(max_plot_count / 10)
  }
  breaks_y <- seq(0, max_plot_count, by = step_y)

  # --- x-range and breaks --------------------------------------------------
  if (is_factor) {
    factor_levels_num <- as.numeric(factor_levels)
    rng_x <- range(factor_levels_num)
    x_limits <- rng_x
    breaks_range <- if (!is.null(xrange)) xrange else x_limits
    if (is.null(xbreaks)) {
      breaks_x <- factor_levels_num
    } else if (is.numeric(xbreaks) && length(xbreaks) == 1L) {
      breaks_x <- pretty(breaks_range, n = xbreaks)
    } else {
      breaks_x <- xbreaks
    }
  } else {
    rng_x <- range(x_vec)
    if (diff(rng_x) == 0) rng_x <- rng_x + c(-0.5, 0.5)
    x_limits <- rng_x
    breaks_range <- if (!is.null(xrange)) xrange else x_limits
    if (is.null(xbreaks)) {
      breaks_x <- pretty(breaks_range, n = 8)
    } else if (is.numeric(xbreaks) && length(xbreaks) == 1L) {
      breaks_x <- pretty(breaks_range, n = xbreaks)
    } else {
      breaks_x <- xbreaks
    }
  }

  p <- ggplot2::ggplot()

  # --- unit rectangles -----------------------------------------------------
  if (bars != "solid") {
    p <- p + ggplot2::layer(
      geom = GeomSquareplot, stat = StatSquareplot, data = data.frame(x = x_vec),
      mapping = ggplot2::aes(x = .data$x), position = "identity",
      inherit.aes = FALSE, show.legend = FALSE,
      params = list(
        binwidth = binwidth, origin = origin,
        fill = fill, colour = "white", alpha = alpha, na.rm = na.rm
      )
    )
  }

  # --- bar outlines / solid bars -------------------------------------------
  if (bars %in% c("outline", "solid") && nrow(bar_df) > 0) {
    outline_color <- if (color == "black") "grey20" else color
    p <- p + ggplot2::geom_rect(
      data = bar_df,
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = 0, ymax = .data$count
      ),
      fill = if (bars == "solid") fill else NA,
      color = outline_color,
      linewidth = 0.5,
      alpha = if (bars == "solid") alpha else 1
    )
  }

  # --- mean line -----------------------------------------------------------
  if (show_mean) {
    mean_val <- mean(x_vec)
    line_top <- if (show_dgp) {
      max_plot_count + extra_top * 0.40
    } else {
      max_plot_count
    }
    p <- p + ggplot2::geom_segment(
      ggplot2::aes(
        x = mean_val, xend = mean_val,
        y = 0, yend = line_top
      ),
      color = "#E60000", linetype = "longdash", linewidth = 0.7
    )
  }

  x_lab <- if (show_dgp) "" else x_label

  base_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(
        color = if (show_dgp) dgp_color else "black"
      ),
      axis.line.y = if (show_dgp) {
        ggplot2::element_blank()
      } else {
        ggplot2::element_line(color = "black")
      },
      axis.text.x = ggplot2::element_text(
        color = if (show_dgp) dgp_color else "black"
      ),
      axis.title.x = ggplot2::element_text(
        color = if (show_dgp) dgp_color else "black"
      ),
      plot.margin = if (show_dgp) {
        ggplot2::margin(5, 5, 30, 5)
      } else {
        ggplot2::margin(5, 5, 5, 5)
      },
      panel.grid.minor.y = ggplot2::element_blank()
    )

  p <- p +
    ggplot2::labs(x = x_lab, y = "count") +
    ggplot2::scale_y_continuous(
      limits = c(0, y_upper),
      breaks = breaks_y,
      labels = breaks_y
    ) +
    ggplot2::scale_x_continuous(limits = xrange, breaks = breaks_x) +
    base_theme +
    ggplot2::coord_cartesian(clip = "off")

  x_min <- if (!is.null(xrange)) xrange[1] else x_limits[1]
  x_max <- if (!is.null(xrange)) xrange[2] else x_limits[2]

  # --- DGP overlay ---------------------------------------------------------
  if (show_dgp) {
    axis_y <- max_plot_count + extra_top * 0.40
    eq_y <- max_plot_count + extra_top * 0.70
    title_y <- max_plot_count + extra_top * 0.98

    p <- p +
      ggplot2::annotate(
        "segment", x = -Inf, xend = Inf,
        y = axis_y, yend = axis_y,
        color = dgp_color, linewidth = 0.5
      ) +
      ggplot2::annotate(
        "text", x = -Inf, y = title_y,
        label = "Population Parameter (DGP)",
        hjust = -0.01, vjust = 0,
        size = 4, fontface = "bold", color = dgp_color
      ) +
      ggplot2::annotate(
        "text", x = -Inf, y = eq_y,
        label = "Y[i] == beta[0] + beta[1] * X[i] + epsilon[i]",
        parse = TRUE, hjust = -0.01, vjust = 0.5,
        size = 4, fontface = "bold", color = dgp_color
      )

    if (0 >= x_min && 0 <= x_max) {
      triangle_y <- axis_y + extra_top * 0.16
      label_y <- axis_y + extra_top * 0.48

      p <- p +
        ggplot2::annotate(
          "point", x = 0, y = triangle_y,
          shape = 25, size = 4,
          color = "#E60000", fill = "#E60000"
        ) +
        ggplot2::annotate(
          "text", x = 0, y = label_y,
          label = "beta[1] == 0", parse = TRUE,
          size = 5, fontface = "bold", color = "#E60000"
        )
    }

    # Bottom x-axis annotations
    p <- p +
      ggplot2::annotate(
        "text", x = -Inf, y = -Inf,
        label = "Parameter Estimate",
        hjust = -0.01, vjust = 3.2,
        size = 4, fontface = "bold", color = dgp_color
      ) +
      ggplot2::annotate(
        "text", x = -Inf, y = -Inf,
        label = "Y[i] == b[0] + b[1] * X[i] + e[i]",
        parse = TRUE, hjust = -0.01, vjust = 4.0,
        size = 4, fontface = "bold", color = dgp_color
      )

    if (0 >= x_min && 0 <= x_max) {
      p <- p + ggplot2::annotate(
        "text", x = 0, y = -Inf, vjust = 2.5,
        label = "b[1]", parse = TRUE,
        size = 5, fontface = "bold", color = dgp_color
      )
    }
  }

  class(p) <- c("gf_squareplot", class(p))
  p
}

#' @export
print.gf_squareplot <- function(x, ...) {
  suppressWarnings(NextMethod("print", x, ...))
  invisible(x)
}
