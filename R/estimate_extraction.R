#' Extract estimates/statistics from a model
#'
#' This collection of functions is useful for extracting estimates and statistics from a fitted
#' model. They are particularly useful when estimating many models, like when bootstrapping
#' confidence intervals. Each function can be used with an already fitted model as an [`lm`] object,
#' or a formula and associated data can be passed to it. **All of these assume the comparison is the
#' empty model.**
#'
#' - **`b0`**: The intercept from the full model.
#' - **`b1`**: The slope b1 from the full model.
#' - **`b`**: The coefficients from the full model.
#' - **`f`**: The F value from the full model.
#' - **`pre`**: The Proportional Reduction in Error for the full model.
#' - **`p`**: The *p*-value from the full model.
#' - **`sse`**: The SS Error (SS Residual) from the model.
#' - **`ssm`**: The SS Model (SS Regression) for the full model.
#' - **`ssr`**: Alias for SSM.
#'
#' @param object A [`lm`] object, or [`formula`].
#' @param data If `object` is a formula, the data to fit the formula to as a [`data.frame`].
#' @param all If `TRUE`, return a named list of all related terms (e.g. all *F*-values).The name
#'   for the full model value is the name of the function (e.g. "f"), and the names for the
#'   constituent terms are the term names prefixed by the function name (e.g. "f_a:b" for the
#'   *F*-value of the `a:b` interaction term).
#' @param predictor Filter the output down to just the statistics for these terms (e.g. "hp" to
#'   just get the statistics for that term in the model). This argument is flexible: you can pass
#'   a character vector of terms (`c("hp", "hp:cyl")`), a one-sided formula (`~hp`), or a list of
#'   formulae (`c(~hp, ~hp:cyl)`).
#' @inherit supernova::supernova
#'
#' @return The value of the estimate as a single number.
#'
#' @importFrom stats lm
#' @name estimate_extraction

#' @rdname estimate_extraction
#' @export
b0 <- function(object, data = NULL) {
  fit <- convert_lm(object, data)
  fit$coefficients[[1]]
}

#' @rdname estimate_extraction
#' @export
b1 <- function(object, data = NULL) {
  fit <- convert_lm(object, data)
  fit$coefficients[[2]]
}

#' @rdname estimate_extraction
#' @export
b <- function(object, data = NULL, all = FALSE, predictor = character()) {
  predictor <- convert_predictor(predictor)
  check_extract_args(all, predictor)
  fit <- convert_lm(object, data)

  # all is unused because there is no concept of an overall b
  # by default we just return all the available b's, and filter to requested terms
  lst <- as.list(fit$coefficients)
  nms <- paste("b", names(lst), sep = "_")
  nms[[1]] <- "b_0"
  out <- set_names(lst, nms)

  if (length(predictor) == 1) {
    out[[which(names(fit$coefficients) %in% predictor)]]
  } else if (length(predictor) > 1) {
    as.list(out[names(fit$coefficients) %in% predictor])
  } else {
    as.list(out[!is.na(out)])
  }
}

#' @rdname estimate_extraction
#' @export
f <- function(object, data = NULL, all = FALSE, predictor = character(), type = 3) {
  predictor <- convert_predictor(predictor)
  check_extract_args(all, predictor, type)
  fit <- convert_lm(object, {{ data }})
  suppressMessages(check_empty_model(fit))
  stats <- extract_stat(fit, type, "F", predictor)
  if (all || !is_empty(predictor)) stats else stats[[1]]
}

#' @rdname estimate_extraction
#' @export
pre <- function(object, data = NULL, all = FALSE, predictor = character(), type = 3) {
  predictor <- convert_predictor(predictor)
  check_extract_args(all, predictor, type)
  fit <- convert_lm(object, {{ data }})
  check_empty_model(fit)
  stats <- extract_stat(fit, type, "PRE", predictor)
  if (all || !is_empty(predictor)) stats else stats[[1]]
}

#' @rdname estimate_extraction
#' @export
p <- function(object, data = NULL, all = FALSE, predictor = character(), type = 3) {
  predictor <- convert_predictor(predictor)
  check_extract_args(all, predictor, type)
  fit <- convert_lm(object, {{ data }})
  check_empty_model(fit)
  stats <- extract_stat(fit, type, "p", predictor)
  if (all || !is_empty(predictor)) stats else stats[[1]]
}

#' Convert a potentially complex predictor to a character vector of terms.
#'
#' @param predictor The predictor(s) to return estimates for.
#'
#' @return A character vector of terms.
#'
#' @noRd
convert_predictor <- function(predictor) {
  purrr::map_if(c(predictor), is_formula, ~ deparse(f_rhs(.x))) %>%
    purrr::flatten_chr()
}

#' Convert a formula and data to an [`lm`] object.
#'
#' @param object A [`lm`] object, or [`formula`].
#' @param data If `object` is a formula, the data to fit the formula to as a [`data.frame`].
#'
#' @return An [`lm`] object.
#'
#' @noRd
convert_lm <- function(object, data) {
  if ("lm" %in% class(object) == FALSE) {
    data_call <- rlang::enquo(data)
    data_name <- rlang::quo_name(data_call)
    call <- paste0("lm(formula = ", deparse(object), ", data = ", data_name, ")")
    fit <- lm(object, data)
    fit$call <- str2lang(call)
  } else {
    fit <- object
  }
  fit
}

#' Assert that the arguments to the estimate extraction functions are valid.
#'
#' @param all Whether to return all the estimates (e.g. all *F*-values).
#' @param predictor The predictor(s) to return estimates for.
#' @param type The type (1, 2, 3) of sums of squares to use.
#'
#' @return Nothing, but throws an error if the arguments are invalid.
#'
#' @noRd
check_extract_args <- function(all, predictor, type = 3) {
  vctrs::vec_assert(all, logical(), 1)
  vctrs::vec_assert(predictor, character())
  vctrs::vec_assert(type, numeric(), 1)
}

#' Determine if the fitted model is the empty/null model.
#'
#' @param fit A fitted linear model to pass to supernova.
#'
#' @return Nothing, but throws an error if the model is empty.
#'
#' @noRd
check_empty_model <- function(fit) {
  models <- supernova::generate_models(fit)
  if (length(models) == 0) {
    abort("Can't extract this estimate from an empty model (it doesn't exist).")
  }
}

#' Extract a statistic from a supernova table and name the values
#'
#' @param fit A fitted linear model to pass to supernova.
#' @param type The type (1, 2, 3) of sums of squares to use.
#' @param stat The statistic from the supernova (as named in the `supernova(...)$tbl`).
#' @param predictor Optionally specify which terms to return (instead of all of them).
#'
#' @return A named list (one for each term, where term is the name) of the values of the statistic.
#'
#' @noRd
extract_stat <- function(fit, type, stat, predictor = character(0)) {
  sup_out <- supernova(fit, type)
  vals <- sup_out$tbl[[stat]]
  nms <- paste(tolower(stat), sup_out$tbl$term, sep = "_")
  nms[[1]] <- tolower(stat)
  out <- set_names(vals, nms)

  if (length(predictor) == 1) {
    out[[which(sup_out$tbl$term %in% predictor)]]
  } else if (length(predictor) > 1) {
    as.list(out[sup_out$tbl$term %in% predictor])
  } else {
    as.list(out[!is.na(out)])
  }
}

# nolint start

#' @rdname estimate_extraction
#' @export
fVal <- f

#' @rdname estimate_extraction
#' @export
PRE <- pre

# nolint end
