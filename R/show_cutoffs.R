#' Add Cutoff Markers to a Histogram
#'
#' `r lifecycle::badge("experimental")`
#'
#' Adds downward-pointing triangle markers at the empirical quantile cutoffs on
#' a histogram that uses a distribution part function (`middle()`, `tails()`,
#' `upper()`, `lower()`, or `outer()`) in its fill aesthetic.
#'
#' @param plot A ggplot histogram with `fill` mapped to a distribution part
#'   function, e.g., `fill = ~middle(Thumb, .95)`.
#' @param color Marker/line color. Default `"#1e3a8a"`.
#' @param size Marker size. Default `4`.
#' @param labels Whether to add text annotations explaining the cutoffs.
#'   Default `FALSE`.
#'
#' @return A ggplot object with cutoff markers and optional labels.
#'
#' @export
#' @examples
#' gf_histogram(~Thumb, data = Fingers, fill = ~middle(Thumb, .95)) %>%
#'   show_cutoffs(labels = TRUE)
show_cutoffs <- function(plot, color = "#1e3a8a", size = 4, labels = FALSE) {
  lifecycle::signal_stage("experimental", "show_cutoffs()")

  # Extract the fill aesthetic expression
  fill_expr <- NULL
  if (!is.null(plot$mapping$fill)) {
    fill_expr <- quo_get_expr(plot$mapping$fill)
  }
  if (is.null(fill_expr) && length(plot$layers) > 0) {
    layer <- plot$layers[[1]]
    if (!is.null(layer$mapping$fill)) {
      fill_expr <- quo_get_expr(layer$mapping$fill)
    }
  }
  if (is.null(fill_expr)) {
    abort(paste(
      "Could not find fill aesthetic.",
      "Use fill = ~middle(...), ~upper(...), ~lower(...),",
      "~outer(...), or ~tails(...)."
    ))
  }

  valid_funcs <- c("middle", "upper", "lower", "outer", "tails")
  if (!is.call(fill_expr) ||
      !(as.character(fill_expr[[1]]) %in% valid_funcs)) {
    abort(paste0(
      "Expected fill using middle/upper/lower/outer/tails. Found: ",
      deparse(fill_expr)
    ))
  }
  func_type <- as.character(fill_expr[[1]])

  if (length(fill_expr) < 3) {
    abort(paste0(
      func_type, "() requires at least 2 arguments: ",
      func_type, "(variable, prop)"
    ))
  }
  prop <- eval(fill_expr[[3]])
  if (!is.numeric(prop) || prop <= 0 || prop >= 1) {
    abort(paste0("prop must be between 0 and 1. Found: ", prop))
  }

  # Extract x variable data from plot
  x_data <- NULL
  if (!is.null(plot$mapping$x)) {
    x_var <- as_name(plot$mapping$x)
    if (!is.null(plot$data) && x_var %in% names(plot$data)) {
      x_data <- plot$data[[x_var]]
    }
  }
  if (is.null(x_data) && length(plot$layers) > 0) {
    layer <- plot$layers[[1]]
    if (!is.null(layer$mapping$x)) {
      x_var <- as_name(layer$mapping$x)
      layer_data <- if (!is.null(layer$data) && is.data.frame(layer$data)) {
        layer$data
      } else {
        plot$data
      }
      if (!is.null(layer_data) && x_var %in% names(layer_data)) {
        x_data <- layer_data[[x_var]]
      }
    }
  }
  if (is.null(x_data)) {
    abort("Could not extract variable from plot.")
  }

  x_clean <- x_data[!is.na(x_data)]
  x_sorted <- sort(x_clean)
  n <- length(x_sorted)

  # Calculate cutoffs based on function type
  cutoff_lower <- NULL
  cutoff_upper <- NULL

  if (func_type %in% c("middle", "tails")) {
    alpha_val <- 1 - prop
    lower_idx <- max(1, min(n, floor(alpha_val / 2 * n) + 1))
    upper_idx <- max(1, min(n, ceiling((1 - alpha_val / 2) * n)))
    cutoff_lower <- x_sorted[lower_idx]
    cutoff_upper <- x_sorted[upper_idx]
    tail_prop <- alpha_val / 2
  } else if (func_type == "upper") {
    cutoff_idx <- max(1, min(n, ceiling((1 - prop) * n)))
    cutoff_upper <- x_sorted[cutoff_idx]
    tail_prop <- prop
  } else if (func_type == "lower") {
    cutoff_idx <- max(1, min(n, floor(prop * n) + 1))
    cutoff_lower <- x_sorted[cutoff_idx]
    tail_prop <- prop
  } else if (func_type == "outer") {
    tail_prop <- prop / 2
    lower_idx <- max(1, min(n, floor(tail_prop * n) + 1))
    upper_idx <- max(1, min(n, ceiling((1 - tail_prop) * n)))
    cutoff_lower <- x_sorted[lower_idx]
    cutoff_upper <- x_sorted[upper_idx]
  }

  # Build plot to get axis ranges
  plot_built <- ggplot2::ggplot_build(plot)
  y_range <- plot_built$layout$panel_params[[1]]$y.range
  if (is.null(y_range)) y_range <- c(0, 30)

  arrow_y <- -y_range[2] * 0.06
  line_top_y <- y_range[2] * 0.20
  x_range <- range(x_clean)
  x_span <- x_range[2] - x_range[1]

  tail_label <- format(tail_prop, digits = 3)
  # Clean up common values
  if (tail_prop == 0.025) tail_label <- ".025"
  else if (tail_prop == 0.05) tail_label <- ".05"
  else if (tail_prop == 0.005) tail_label <- ".005"
  else if (tail_prop == 0.01) tail_label <- ".01"
  else if (tail_prop == 0.1) tail_label <- ".10"

  # Add lower cutoff
  if (!is.null(cutoff_lower)) {
    line_bottom_y <- arrow_y + y_range[2] * 0.015
    plot <- plot +
      ggplot2::annotate(
        "segment",
        x = cutoff_lower, xend = cutoff_lower,
        y = line_bottom_y, yend = line_top_y,
        linetype = "dashed", linewidth = 0.5, color = color
      )
    if (labels) {
      label_x <- x_range[1] + x_span * 0.08
      label_y <- y_range[2] * 0.65
      line_end_x <- label_x + x_span * 0.02
      line_end_y <- label_y - y_range[2] * 0.08
      plot <- plot +
        ggplot2::annotate(
          "segment",
          x = cutoff_lower, xend = line_end_x,
          y = line_top_y, yend = line_end_y,
          linetype = "dashed", linewidth = 0.5, color = color
        ) +
        ggplot2::annotate(
          "text", x = label_x, y = label_y,
          label = paste0(tail_label, " of\nvalues below"),
          hjust = 0.5, vjust = 0.5, size = 3.2,
          color = color, fontface = "italic"
        )
    }
    plot <- plot +
      ggplot2::annotate(
        "point", x = cutoff_lower, y = arrow_y,
        shape = 25, size = size, fill = color, color = color
      )
  }

  # Add upper cutoff
  if (!is.null(cutoff_upper)) {
    line_bottom_y <- arrow_y + y_range[2] * 0.015
    plot <- plot +
      ggplot2::annotate(
        "segment",
        x = cutoff_upper, xend = cutoff_upper,
        y = line_bottom_y, yend = line_top_y,
        linetype = "dashed", linewidth = 0.5, color = color
      )
    if (labels) {
      label_x <- x_range[2] - x_span * 0.08
      label_y <- y_range[2] * 0.65
      line_end_x <- label_x - x_span * 0.02
      line_end_y <- label_y - y_range[2] * 0.08
      plot <- plot +
        ggplot2::annotate(
          "segment",
          x = cutoff_upper, xend = line_end_x,
          y = line_top_y, yend = line_end_y,
          linetype = "dashed", linewidth = 0.5, color = color
        ) +
        ggplot2::annotate(
          "text", x = label_x, y = label_y,
          label = paste0(tail_label, " of\nvalues above"),
          hjust = 0.5, vjust = 0.5, size = 3.2,
          color = color, fontface = "italic"
        )
    }
    plot <- plot +
      ggplot2::annotate(
        "point", x = cutoff_upper, y = arrow_y,
        shape = 25, size = size, fill = color, color = color
      )
  }

  plot + ggplot2::coord_cartesian(clip = "off")
}
