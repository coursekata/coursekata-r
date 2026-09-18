#' Re-evaluate shared positional expressions reproducibly on live rows
#'
#' Residuals and their points must evaluate a random expression to the same
#' values. A seeded expression retains its original environment and is evaluated
#' again on every build, so replacing plot data or changing a caller binding
#' still works. Only the returned plot receives these mapping copies.
#' @noRd
stabilize_resid_mappings <- function(plot) {
  if (!inherits(plot, "ggplot")) return(plot)
  source <- plot_source(plot, resolve.data = FALSE)
  seeds <- with_random_seed_restored(sample.int(.Machine$integer.max, 2L))
  names(seeds) <- c("x", "y")
  for (axis in c("x", "y")) {
    original <- source$mapping[[axis]]
    if (is.null(original) || !rlang::is_quosure(original) ||
        !is.null(attr(original, "coursekata_original_mapping")) ||
        rlang::is_symbol(rlang::quo_get_expr(original)) ||
        has_build_time_call(rlang::quo_get_expr(original))) next
    stable <- rlang::new_quosure(
      rlang::expr(.env$.coursekata_resid_eval(!!rlang::quo_get_expr(original))),
      rlang::new_environment(
        list(.coursekata_resid_eval = resid_mapping_evaluator(seeds[[axis]])),
        parent = rlang::quo_get_env(original)
      )
    )
    attr(stable, "coursekata_original_mapping") <- original
    if (identical(plot$mapping[[axis]], original)) plot$mapping[[axis]] <- stable
    for (i in seq_along(plot$layers)) {
      layer <- plot$layers[[i]]
      if (identical(layer$mapping[[axis]], original)) {
        mapping <- layer$mapping
        mapping[[axis]] <- stable
        plot$layers[[i]] <- layer_with(layer, mapping = mapping)
      }
    }
    if (is.null(plot$labels[[axis]]) || is.function(plot$labels[[axis]])) {
      # ggplot2's functional labels receive the current derived label. Restore
      # the user's spelling only while this wrapper still supplies that label.
      plot$labels[[axis]] <- source_label_restore(
        as_label(stable), as_label(original), plot$labels[[axis]]
      )
    }
  }
  plot
}

resid_mapping_evaluator <- function(seed) {
  force(seed)
  function(value) with_fixed_seed(seed, value)
}

#' Refuse an unshared random expression introduced after source binding
#'
#' A later aes() addition can replace the reproducible expression installed at
#' add time. Compare unwrapped expressions under distinct ambient seeds,
#' restoring RNG even when evaluation fails. Matching sampled values alone do
#' not establish reproducibility: small samples can collide. A helper that
#' changes RNG must also finish at the same state under both seeds. When it
#' preserves RNG, additional value probes catch helpers such as preserve_seed.
#' @noRd
check_resid_live_mappings <- function(mapping, data, call = caller_env()) {
  for (axis in intersect(c("x", "y"), names(mapping))) {
    quo <- mapping[[axis]]
    if (!rlang::is_quosure(quo) || rlang::is_symbol(rlang::quo_get_expr(quo)) ||
        !is.null(attr(quo, "coursekata_original_mapping")) ||
        resid_deterministic_expr(rlang::quo_get_expr(quo), rlang::quo_get_env(quo)) ||
        has_build_time_call(rlang::quo_get_expr(quo))) next
    probe <- function(seed) with_fixed_seed(seed, {
      before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      value <- eval_tidy(quo, data)
      after <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      list(value = value, state = after, changed = !identical(before, after))
    })
    probes <- lapply(1:2, probe)
    same_values <- function() all(vapply(probes[-1], function(x) {
      identical(x$value, probes[[1]]$value)
    }, logical(1)))
    if (!any(vapply(probes, `[[`, logical(1), "changed")) && same_values()) {
      probes <- c(probes, lapply(3:8, probe))
    }
    changed <- any(vapply(probes, `[[`, logical(1), "changed"))
    same_states <- all(vapply(probes[-1], function(x) {
      identical(x$state, probes[[1]]$state)
    }, logical(1)))
    random <- !same_values() || (changed && !same_states)
    if (random) {
      abort(c(
        glue("The residual layer cannot share the random `{axis}` mapping: {as_label(quo)}."),
        i = "Add this mapping to the plot or observation layer before adding the residual layer.",
        i = "Alternatively, give the expression its own fixed random seed."
      ), call = call)
    }
  }
}

#' Avoid extra evaluation of ordinary deterministic base transformations
#' @noRd
resid_deterministic_expr <- function(expr, env) {
  if (!rlang::is_call(expr)) return(TRUE)
  if (!rlang::is_symbol(expr[[1]])) return(FALSE)
  name <- rlang::as_string(expr[[1]])
  known <- c("+", "-", "*", "/", "^", "(", "I", "log", "log10", "log2", "exp",
             "sqrt", "abs", "round", "floor", "ceiling", "as.numeric", "as.double")
  if (!name %in% known || !identical(get0(name, env, mode = "function", inherits = TRUE),
                                    get(name, baseenv()))) return(FALSE)
  all(vapply(as.list(expr)[-1], resid_deterministic_expr, logical(1), env = env))
}

#' @export
ggplot_add.coursekata_resid_layer <- function(object, plot, ...) {
  plot <- stabilize_resid_mappings(plot)
  object <- layer_with(object)
  class(object) <- setdiff(class(object), "coursekata_resid_layer")
  ggplot_add.coursekata_source_layer(object, plot, ...)
}
