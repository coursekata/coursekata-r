# show_cutoffs.R
# A single pipe-friendly function to add arrow markers at alpha cutoff points
# For use with coursekata package and ggformula histograms

#' Add arrow markers at alpha cutoff points on a histogram
#'
#' Pipe this function after a gf_histogram() call to add downward-pointing
#' triangular markers at the empirical quantile cutoffs. The function
#' automatically extracts both the variable and the prop value from the
#' middle(), upper(), lower(), outer(), or tails() call in your fill aesthetic.
#'
#' @param plot A ggplot object (typically a histogram created with gf_histogram
#'        with fill = ~middle(...), ~upper(...), ~lower(...), ~outer(...), or ~tails(...))
#' @param color Color of the arrow markers (default: "#1e3a8a" - dark blue)
#' @param size Size of the arrow markers (default: 4)
#' @param labels Logical; whether to add text annotations explaining the cutoffs
#'        (default: FALSE)
#'
#' @return A ggplot object with arrow markers added
#'
#' @export
#' @examples
#' library(coursekata)
#'
#' # Two-tailed middle: middle() - shades center, markers on both sides
#' gf_histogram(~Thumb, data = Fingers, fill = ~middle(Thumb, .95)) %>%
#'   show_cutoffs(labels = TRUE)
#'
#' # Two-tailed outer: outer() - shades both tails, markers on both sides
#' gf_histogram(~Thumb, data = Fingers, fill = ~outer(Thumb, .05)) %>%
#'   show_cutoffs(labels = TRUE)
#'
#' # One-tailed upper: upper() - marker on right only
#' gf_histogram(~Thumb, data = Fingers, fill = ~upper(Thumb, .05)) %>%
#'   show_cutoffs(labels = TRUE)
#'
#' # One-tailed lower: lower() - marker on left only
#' gf_histogram(~Thumb, data = Fingers, fill = ~lower(Thumb, .05)) %>%
#'   show_cutoffs(labels = TRUE)
#'
show_cutoffs <- function(plot, color = "#1e3a8a", size = 4, labels = FALSE) {

  # Extract the fill aesthetic to find the middle/upper/lower call
  fill_expr <- NULL

  # Try plot's main mapping first
  if (!is.null(plot$mapping$fill)) {
    fill_expr <- rlang::quo_get_expr(plot$mapping$fill)
  }

  # Try first layer's mapping
  if (is.null(fill_expr) && length(plot$layers) > 0) {
    layer <- plot$layers[[1]]
    if (!is.null(layer$mapping$fill)) {
      fill_expr <- rlang::quo_get_expr(layer$mapping$fill)
    }
  }

  if (is.null(fill_expr)) {
    stop("Could not find fill aesthetic. Make sure you have fill = ~middle(...), ~upper(...), ~lower(...), ~outer(...), or ~tails(...) in your histogram.")
  }

  # Determine which function is being used
  valid_funcs <- c("middle", "upper", "lower", "outer", "tails")
  if (!is.call(fill_expr) || !(as.character(fill_expr[[1]]) %in% valid_funcs)) {
    stop("Expected fill = ~middle(...), ~upper(...), ~lower(...), ~outer(...), or ~tails(...). Found: ", deparse(fill_expr))
  }
  
  func_type <- as.character(fill_expr[[1]])

  # Extract the prop argument (second argument)
  if (length(fill_expr) < 3) {
    stop(func_type, "() requires at least 2 arguments: ", func_type, "(variable, prop)")
  }

  prop <- eval(fill_expr[[3]])

  if (!is.numeric(prop) || prop <= 0 || prop >= 1) {
    stop("prop must be a number between 0 and 1. Found: ", prop)
  }

  # Extract the x variable data
  x_data <- NULL

  # Try to get from the plot's main mapping
  if (!is.null(plot$mapping$x)) {
    x_var <- rlang::as_name(plot$mapping$x)
    if (!is.null(plot$data) && x_var %in% names(plot$data)) {
      x_data <- plot$data[[x_var]]
    }
  }

  # If not found, try from the first layer
  if (is.null(x_data) && length(plot$layers) > 0) {
    layer <- plot$layers[[1]]

    if (!is.null(layer$mapping$x)) {
      x_var <- rlang::as_name(layer$mapping$x)
      layer_data <- if (!is.null(layer$data) && is.data.frame(layer$data)) {
        layer$data
      } else if (!is.null(plot$data)) {
        plot$data
      } else {
        NULL
      }
      if (!is.null(layer_data) && x_var %in% names(layer_data)) {
        x_data <- layer_data[[x_var]]
      }
    }
  }

  if (is.null(x_data)) {
    stop("Could not extract variable from plot. Make sure you're piping from a gf_histogram() call.")
  }

  # Calculate cutoffs based on function type
  x_clean <- x_data[!is.na(x_data)]
  x_sorted <- sort(x_clean)
  n <- length(x_sorted)
  
  if (func_type == "middle" || func_type == "tails") {
    # Two-tailed: cutoffs on both sides
    # middle(x, .95) and tails(x, .95) have the same cutoffs
    alpha <- 1 - prop
    lower_idx <- floor(alpha / 2 * n) + 1
    upper_idx <- ceiling((1 - alpha / 2) * n)
    lower_idx <- max(1, min(n, lower_idx))
    upper_idx <- max(1, min(n, upper_idx))
    cutoff_lower <- x_sorted[lower_idx]
    cutoff_upper <- x_sorted[upper_idx]
    tail_prop <- alpha / 2
    
  } else if (func_type == "upper") {
    # One-tailed upper: cutoff on right only
    # upper(x, .05) shades the top 5%, so cutoff is at the 95th percentile
    cutoff_idx <- ceiling((1 - prop) * n)
    cutoff_idx <- max(1, min(n, cutoff_idx))
    cutoff_lower <- NULL
    cutoff_upper <- x_sorted[cutoff_idx]
    tail_prop <- prop
    
  } else if (func_type == "lower") {
    # One-tailed lower: cutoff on left only
    # lower(x, .05) shades the bottom 5%, so cutoff is at the 5th percentile
    cutoff_idx <- floor(prop * n) + 1
    cutoff_idx <- max(1, min(n, cutoff_idx))
    cutoff_lower <- x_sorted[cutoff_idx]
    cutoff_upper <- NULL
    tail_prop <- prop
    
  } else if (func_type == "outer") {
    # Two-tailed outer: cutoffs on both sides (shades both tails)
    # outer(x, .05) shades the outer 5% total (2.5% in each tail)
    # This is the complement of middle(x, .95)
    tail_prop <- prop / 2
    lower_idx <- floor(tail_prop * n) + 1
    upper_idx <- ceiling((1 - tail_prop) * n)
    lower_idx <- max(1, min(n, lower_idx))
    upper_idx <- max(1, min(n, upper_idx))
    cutoff_lower <- x_sorted[lower_idx]
    cutoff_upper <- x_sorted[upper_idx]
  }

  # Build the plot to get y-axis range (needed for arrow positioning)
  plot_built <- ggplot2::ggplot_build(plot)
  y_range <- plot_built$layout$panel_params[[1]]$y.range
  if (is.null(y_range)) y_range <- c(0, 30)
  
  # Position arrows below x-axis (in margin)
  arrow_y <- -y_range[2] * 0.06
  
  # Consistent height for dashed lines (about 20% up the plot)
  line_top_y <- y_range[2] * 0.20
  
  # X range for positioning labels
  x_range <- range(x_clean)
  x_span <- x_range[2] - x_range[1]
  
  # Format the proportion nicely for labels
  tail_label <- if (tail_prop == 0.025) ".025" 
                else if (tail_prop == 0.05) ".05"
                else if (tail_prop == 0.005) ".005"
                else if (tail_prop == 0.01) ".01"
                else if (tail_prop == 0.1) ".10"
                else format(tail_prop, digits = 3)
  
  # Add dashed lines for lower cutoff (drawn first, arrows on top)
  if (!is.null(cutoff_lower)) {
    # Dashed vertical line - stops just above the arrow tip
    line_bottom_y <- arrow_y + y_range[2] * 0.015
    plot <- plot +
      ggplot2::annotate("segment", 
                        x = cutoff_lower, xend = cutoff_lower,
                        y = line_bottom_y, yend = line_top_y,
                        linetype = "dashed", linewidth = 0.5,
                        color = color)
    
    # Label with angled dashed line (only if labels=TRUE)
    if (labels) {
      # Fixed label position in upper-left white space (based on data range, not cutoff)
      label_x <- x_range[1] + x_span * 0.08
      label_y <- y_range[2] * 0.65
      
      # Line endpoint stops before reaching the label
      line_end_x <- label_x + x_span * 0.02
      line_end_y <- label_y - y_range[2] * 0.08
      
      plot <- plot +
        # Angled dashed line from top of vertical line toward label
        ggplot2::annotate("segment",
                          x = cutoff_lower, xend = line_end_x,
                          y = line_top_y, yend = line_end_y,
                          linetype = "dashed", linewidth = 0.5,
                          color = color) +
        ggplot2::annotate("text",
                          x = label_x,
                          y = label_y,
                          label = paste0(tail_label, " of\nvalues below"),
                          hjust = 0.5, vjust = 0.5, size = 3.2, color = color,
                          fontface = "italic")
    }
    
    # Arrow below x-axis (drawn last, on top of line)
    plot <- plot +
      ggplot2::annotate("point", x = cutoff_lower, y = arrow_y,
                        shape = 25, size = size, fill = color, color = color)
  }
  
  # Add dashed lines for upper cutoff (drawn first, arrows on top)
  if (!is.null(cutoff_upper)) {
    # Dashed vertical line - stops just above the arrow tip
    line_bottom_y <- arrow_y + y_range[2] * 0.015
    plot <- plot +
      ggplot2::annotate("segment",
                        x = cutoff_upper, xend = cutoff_upper,
                        y = line_bottom_y, yend = line_top_y,
                        linetype = "dashed", linewidth = 0.5,
                        color = color)
    
    # Label with angled dashed line (only if labels=TRUE)
    if (labels) {
      # Fixed label position in upper-right white space (based on data range, not cutoff)
      label_x <- x_range[2] - x_span * 0.08
      label_y <- y_range[2] * 0.65
      
      # Line endpoint stops before reaching the label
      line_end_x <- label_x - x_span * 0.02
      line_end_y <- label_y - y_range[2] * 0.08
      
      plot <- plot +
        # Angled dashed line from top of vertical line toward label
        ggplot2::annotate("segment",
                          x = cutoff_upper, xend = line_end_x,
                          y = line_top_y, yend = line_end_y,
                          linetype = "dashed", linewidth = 0.5,
                          color = color) +
        ggplot2::annotate("text",
                          x = label_x,
                          y = label_y,
                          label = paste0(tail_label, " of\nvalues above"),
                          hjust = 0.5, vjust = 0.5, size = 3.2, color = color,
                          fontface = "italic")
    }
    
    # Arrow below x-axis (drawn last, on top of line)
    plot <- plot +
      ggplot2::annotate("point", x = cutoff_upper, y = arrow_y,
                        shape = 25, size = size, fill = color, color = color)
  }
  
  # Allow drawing in margins
  plot <- plot + ggplot2::coord_cartesian(clip = "off")
  
  plot
}