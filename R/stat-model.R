#' Compute the model represented by a layer
#'
#' `StatModel` passes through rows prepared from a supplied model. With no
#' supplied model, it fits the model implied by the mapped positions: a linear
#' model for a continuous predictor, group means for a categorical predictor,
#' or an intercept when only the outcome is mapped.
#'
#' @format `StatModel` is a [ggplot2::Stat] object.
#' @export
StatModel <- ggplot2::ggproto(
  "StatModel", ggplot2::Stat,
  required_aes = "x|y",
  optional_aes = ".model_kind",
  dropped_aes = "weight",
  extra_params = c(
    "na.rm", "orientation", "formula", "se", "n", "fullrange",
    "level", "method.args"
  ),
  setup_params = function(data, params) {
    params[["flipped_aes"]] <- if (!is.null(params[["orientation"]]) &&
      !is.na(params[["orientation"]])) {
      identical(params[["orientation"]], "y")
    } else if ("x" %in% names(data) && !"y" %in% names(data)) {
      TRUE
    } else {
      FALSE
    }
    params[["formula"]] <- params[["formula"]] %||% (y ~ x)
    params[["se"]] <- params[["se"]] %||% FALSE
    params[["n"]] <- params[["n"]] %||% 80L
    params[["fullrange"]] <- params[["fullrange"]] %||% FALSE
    params[["level"]] <- params[["level"]] %||% 0.95
    params[["method.args"]] <- params[["method.args"]] %||% list()
    params
  },
  compute_group = function(data, scales, formula = y ~ x, se = FALSE,
                           n = 80L, fullrange = FALSE, level = 0.95,
                           method.args = list(), na.rm = FALSE,
                           flipped_aes = FALSE) {
    explicit <- if (".model_kind" %in% names(data)) {
      unique(stats::na.omit(data$.model_kind))
    } else {
      character()
    }
    if (length(explicit)) {
      kind <- as.character(explicit[[1L]])
      data$flipped_aes <- flipped_aes
      if (identical(kind, "hline")) data$yintercept <- data$y
      if (identical(kind, "vline")) data$xintercept <- data$x
      return(data)
    }

    has_x <- "x" %in% names(data)
    has_y <- "y" %in% names(data)
    if (has_x && !has_y) {
      return(data.frame(
        x = mean(data$x, na.rm = TRUE), xintercept = mean(data$x, na.rm = TRUE),
        .model_kind = "vline", flipped_aes = TRUE
      ))
    }
    if (has_y && !has_x) {
      return(data.frame(
        y = mean(data$y, na.rm = TRUE), yintercept = mean(data$y, na.rm = TRUE),
        .model_kind = "hline", flipped_aes = FALSE
      ))
    }

    canonical <- ggplot2::flip_data(data, flipped_aes)
    if (inherits(canonical$x, "mapped_discrete")) {
      result <- data.frame(
        x = canonical$x[[1L]], y = mean(canonical$y, na.rm = TRUE),
        group = canonical$group[[1L]], .model_kind = "segment",
        flipped_aes = flipped_aes
      )
      return(ggplot2::flip_data(result, flipped_aes))
    }

    result <- ggplot2::StatSmooth$compute_group(
      data, scales, method = stats::lm, formula = formula, se = se,
      n = n, fullrange = fullrange, level = level,
      method.args = method.args, na.rm = na.rm,
      flipped_aes = flipped_aes
    )
    if (is.null(result) || nrow(result) == 0L) return(result)
    result$.model_kind <- "line"
    result
  }
)
