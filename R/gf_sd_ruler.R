#' Add a Standard Deviation Ruler to a Plot
#'
#' `r lifecycle::badge("experimental")`
#'
#' Adds a segment showing one standard deviation of a variable, anchored at
#' the mean. The orientation depends on where the outcome variable lives:
#' on a scatter or jitter plot (outcome on the y-axis) the ruler is a
#' vertical segment placed at a chosen x position; on a histogram (outcome
#' on the x-axis, no y aesthetic) it is a horizontal segment running from
#' the mean to mean + SD along the baseline. The orientation is detected
#' automatically from the plot's axis mappings.
#'
#' @param p A ggplot object (typically from `gf_point()`, `gf_jitter()`, or
#'   `gf_histogram()`).
#' @param y The y-variable (bare name or string). Defaults to the plot's
#'   mapped y aesthetic if omitted.
#' @param data Dataset. Defaults to `p$data`.
#' @param x The x-variable (bare name or string). On scatter and jitter
#'   plots this controls the ruler's placement; on histograms it is the
#'   outcome variable. Defaults to the plot's mapped x.
#' @param where For vertical rulers, where on the x-axis to place the ruler:
#'   `"middle"` (midpoint of x range), `"mean"`, or `"median"`. Ignored for
#'   histograms, where the ruler always starts at the mean.
#' @param color Segment color. Default `"red"`.
#' @param size Segment `linewidth`. Default `0.8`.
#' @param ... Additional arguments passed to [ggplot2::geom_segment()].
#'
#' @return A ggplot object with the SD ruler segment added.
#'
#' @export
#' @seealso
#' The model visualization guide shows the ruler alongside residuals and
#' compares groups with different spread:
#' <https://coursekata.github.io/coursekata-r/articles/model-visualization.html>
#'
#' @examples
#' # the ruler runs from the mean (the empty model) up by one standard
#' # deviation -- it looks like a residual because SD is a typical residual
#' gf_point(Thumb ~ Height, data = Fingers, alpha = .4) %>%
#'   gf_model(lm(Thumb ~ NULL, data = Fingers)) %>%
#'   gf_sd_ruler()
#'
#' # `where` controls placement along the x-axis
#' gf_point(Thumb ~ Height, data = Fingers, alpha = .4) %>%
#'   gf_sd_ruler(where = "mean")
#'
#' # categorical x works the same way
#' gf_jitter(Thumb ~ Sex, data = Fingers, width = .1, alpha = .4) %>%
#'   gf_sd_ruler(where = "median")
#'
#' # on a histogram the outcome is on the x-axis, so the ruler is horizontal
#' # and runs along the baseline from the mean to one SD above it
#' gf_histogram(~Thumb, data = Fingers) %>%
#'   gf_sd_ruler(color = "red", size = 2)
#'
#' # name the variable explicitly when the plot does not make it obvious
#' gf_point(Thumb ~ Height, data = Fingers, alpha = .4) %>%
#'   gf_sd_ruler(y = Thumb)
gf_sd_ruler <- function(p, y = NULL, data = NULL, x = NULL,
                        where = c("middle", "mean", "median"),
                        color = "red", size = 0.8, ...) {
  lifecycle::signal_stage("experimental", "gf_sd_ruler()")
  where <- match.arg(where)
  spec <- plot_spec(p)
  if (is.null(data)) data <- spec$data

  # capture bare variable names before any evaluation forces them
  y_arg <- arg_name(substitute(y), function() y, data)
  x_arg <- arg_name(substitute(x), function() x, data)

  # histogram mode: no y aesthetic and no explicit y, so the outcome is on
  # the x-axis and the ruler is a horizontal segment along the baseline
  if (is.null(y_arg) && is.null(spec$mapping$y)) {
    if (is.null(x_arg)) {
      if (is.null(spec$mapping$x)) {
        abort("Can't infer the outcome variable; please pass y or x explicitly.")
      }
      x_name <- mapped_name(spec$mapping$x, "x")
    } else {
      x_name <- x_arg
    }

    x_vals <- outcome_values(data, x_name)
    m <- mean(x_vals, na.rm = TRUE)
    s <- stats::sd(x_vals, na.rm = TRUE)

    seg <- data.frame(x = m, xend = m + s, y = 0, yend = 0)
    return(p + tag_layer(
      ggplot2::geom_segment(
        data = seg,
        mapping = ggplot2::aes(
          x = .data$x, xend = .data$xend,
          y = .data$y, yend = .data$yend
        ),
        inherit.aes = FALSE,
        color = color,
        linewidth = size,
        ...
      ),
      "sd_ruler"
    ))
  }

  # scatter/jitter mode: outcome on the y-axis, vertical ruler at a chosen x
  y_name <- if (is.null(y_arg)) mapped_name(spec$mapping$y, "y") else y_arg

  y_vals <- outcome_values(data, y_name)
  m <- mean(y_vals, na.rm = TRUE)
  s <- stats::sd(y_vals, na.rm = TRUE)

  # infer x for placement; unlike the outcome this may be categorical
  if (is.null(x_arg)) {
    if (!is.null(spec$mapping$x)) {
      x_name <- mapped_name(spec$mapping$x, "x")
      x_vals_raw <- column(data, x_name)
    } else {
      x_vals_raw <- seq_along(y_vals)
    }
  } else {
    x_vals_raw <- column(data, x_arg)
  }

  # turn categorical x into numeric positions
  x_vals <- x_vals_raw
  if (!is.numeric(x_vals)) {
    if (is.factor(x_vals)) {
      x_vals <- as.numeric(x_vals)
    } else {
      x_vals <- as.numeric(factor(x_vals, levels = unique(x_vals)))
    }
  }

  # compute placement
  x0 <- switch(where,
    middle = (min(x_vals, na.rm = TRUE) + max(x_vals, na.rm = TRUE)) / 2,
    mean   = mean(x_vals, na.rm = TRUE),
    median = stats::median(x_vals, na.rm = TRUE)
  )

  seg <- data.frame(x = x0, xend = x0, y = m, yend = m + s)

  p +
    tag_layer(
      ggplot2::geom_segment(
        data = seg,
        mapping = ggplot2::aes(
          x = .data$x, xend = .data$xend,
          y = .data$y, yend = .data$yend
        ),
        inherit.aes = FALSE,
        color = color,
        linewidth = size,
        ...
      ),
      "sd_ruler"
    )
}

#' Resolve `Thumb`, `"Thumb"`, or a variable holding `"Thumb"` to a column name
#'
#' Reads the data before the caller's environment, so an object of the same
#' name in scope can't shadow a real column.
#'
#' @noRd
arg_name <- function(expr, value, data, call = caller_env()) {
  if (is.null(expr)) {
    return(NULL)
  }
  if (!is_symbol(expr)) {
    return(value())
  }

  name <- deparse(expr)
  if (name %in% names(data)) {
    return(name)
  }

  held <- tryCatch(value(), error = function(e) NULL)
  if (is.character(held) && length(held) == 1L) held else name
}

#' Read a variable name out of a plot's aesthetic mapping
#'
#' A computed mapping like `~log(Thumb)` is a call, not a name, and bare
#' `as_name()` would fail on it without mentioning this function.
#'
#' @noRd
mapped_name <- function(mapping, aes_name, call = caller_env()) {
  if (!is_symbol(quo_get_expr(mapping))) {
    abort(
      c(
        glue("Can't read the {aes_name} variable from the plot's mapping"),
        glue("the plot maps {aes_name} to `{as_label(mapping)}`, not a variable"),
        glue("pass {aes_name} explicitly to say which variable to measure")
      ),
      call = call
    )
  }
  as_name(mapping)
}

#' Pull a column out of the data, by name
#'
#' @noRd
column <- function(data, name, call = caller_env()) {
  if (!name %in% names(data)) {
    abort(
      c(
        glue("Can't find `{name}` in the data"),
        glue("available variables: {collapse(names(data))}")
      ),
      call = call
    )
  }
  data[[name]]
}

#' Pull the quantitative column the ruler measures
#'
#' @noRd
outcome_values <- function(data, name, call = caller_env()) {
  values <- column(data, name, call = call)
  if (!is.numeric(values)) {
    abort(
      c(
        glue("`{name}` is not a quantitative variable"),
        glue("detected type: {class(values)[[1]]}"),
        "a standard deviation ruler needs a quantitative outcome"
      ),
      call = call
    )
  }
  values
}
