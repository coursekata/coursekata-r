#' Countable-Rectangle Histogram
#'
#' `r lifecycle::badge("experimental")`
#'
#' Creates histograms where individual data points are visible as stacked unit
#' rectangles, making counts easy to visualize. Designed for teaching
#' statistical concepts, particularly sampling distributions.
#'
#' @param x Formula (`~variable`) or numeric vector.
#' @param data Data frame (required if `x` is a formula).
#' @param binwidth Width of histogram bins. Auto-calculated if `NULL`.
#' @param origin Starting position for bins.
#' @param boundary Alias for `origin`.
#' @param fill Rectangle fill color. Default `"#7fcecc"`.
#' @param color Rectangle border color. Default `"black"`.
#' @param alpha Transparency. Default `1`.
#' @param na.rm Remove `NA` values. Default `TRUE`.
#' @param mincount Minimum y-axis height for consistent scaling.
#' @param bars Display style: `"none"` (squares only), `"outline"`, or
#'   `"solid"`.
#' @param xbreaks Number of x-axis breaks or vector of specific positions.
#' @param xrange X-axis limits as `c(min, max)`.
#' @param show_dgp Show DGP annotation overlay. Default `FALSE`.
#' @param show_mean Show dashed mean line. Default `FALSE`.
#' @param auto_subdivide Split bins with >75 observations into sub-columns.
#'   Default `FALSE`.
#'
#' @return A ggplot object with S3 class `c("gf_squareplot", "gg", "ggplot")`.
#'
#' @export
#' @examples
#' gf_squareplot(~Thumb, data = Fingers)
#' gf_squareplot(~Thumb, data = Fingers, bars = "outline")
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

  # --- extract x vector ----------------------------------------------------
  is_formula <- inherits(x, "formula")
  if (is_formula) {
    vars <- all.vars(x)
    if (length(vars) != 1L) abort("Formula must be of form ~var.")
    if (is.null(data)) {
      x_raw <- tryCatch(
        get(vars[1], envir = parent.frame()),
        error = function(e) {
          abort(paste0(
            "Variable '", vars[1],
            "' not found. Either supply data= or ensure variable exists."
          ))
        }
      )
    } else {
      x_raw <- data[[vars[1]]]
    }
    x_label <- vars[1]
  } else {
    x_raw <- x
    x_label <- NULL
  }

  # Handle factors
  is_factor <- is.factor(x_raw)
  factor_levels <- NULL
  if (is_factor) {
    factor_levels <- levels(x_raw)
    factor_levels_num <- suppressWarnings(as.numeric(factor_levels))
    if (any(is.na(factor_levels_num))) {
      abort("Factor levels must be convertible to numeric for gf_squareplot")
    }
    x_vec <- as.numeric(as.character(x_raw))
  } else {
    x_vec <- x_raw
  }

  if (na.rm) x_vec <- x_vec[!is.na(x_vec)]
  if (!is.numeric(x_vec)) abort("`x` must be numeric.")
  if (length(x_vec) == 0) abort("`x` has no non-missing values.")

  # --- binwidth ------------------------------------------------------------
  if (is.null(binwidth)) {
    if (is_factor) {
      binwidth <- 1
    } else {
      rng <- range(x_vec)
      if (diff(rng) == 0) {
        binwidth <- 1
      } else {
        is_integer_like <- all(abs(x_vec - round(x_vec)) < 1e-7)
        if (is_integer_like && diff(rng) <= 50) {
          binwidth <- 1
        } else {
          binwidth <- diff(rng) / 30
        }
      }
    }
  }

  # --- origin / boundary ---------------------------------------------------
  if (!is.null(boundary)) {
    origin <- boundary
  } else if (is.null(origin)) {
    if (is_factor) {
      origin <- min(as.numeric(factor_levels))
    } else {
      origin <- floor(min(x_vec) / binwidth) * binwidth
    }
  }

  # --- assign bins ---------------------------------------------------------
  bin <- floor((x_vec - origin) / binwidth)
  counts_per_bin <- table(bin)
  max_in_any_bin <- if (length(counts_per_bin) > 0) max(counts_per_bin) else 0

  if (auto_subdivide && max_in_any_bin > 75) {
    n_cols <- ceiling(max_in_any_bin / 75)
    if (bars == "none") bars <- "outline"
  } else if (!auto_subdivide && max_in_any_bin > 75 &&
               bars %in% c("none", "outline")) {
    n_cols <- 1
    bars <- "solid"
  } else {
    n_cols <- 1
  }

  sub_binwidth <- binwidth / n_cols
  slot <- stats::ave(x_vec, bin, FUN = function(z) seq_along(z) - 1L)
  row_num <- floor(slot / n_cols)
  col_num <- slot %% n_cols

  xmin <- origin + bin * binwidth + col_num * sub_binwidth
  xmax <- xmin + sub_binwidth
  ymin <- row_num
  ymax <- row_num + 1

  rect_df <- data.frame(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                        bin = bin)

  # --- bar counts ----------------------------------------------------------
  if (nrow(rect_df) > 0) {
    bin_summary <- stats::aggregate(ymax ~ bin, rect_df, max)
    names(bin_summary)[2] <- "count"
    bar_df <- data.frame(
      xmin = origin + bin_summary$bin * binwidth,
      xmax = origin + (bin_summary$bin + 1) * binwidth,
      count = bin_summary$count
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
    p <- p + ggplot2::geom_rect(
      data = rect_df,
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax
      ),
      fill = fill, color = "white", alpha = alpha
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
