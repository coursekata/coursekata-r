#' Does an expression compute something only ggplot2's build can supply
#'
#' `after_stat()`, `stat()` and `after_scale()` mark a mapping as a build-time
#' instruction rather than a value: evaluating `after_stat(density)` against a
#' plot's source data does not raise an error, it silently returns the
#' function `stats::density`, which is not a value anyone meant to pin. The
#' expression is walked because the call can be nested inside another one, and
#' `rlang::is_call()` matches a namespace-qualified call
#' (`ggplot2::after_stat(...)`) by function name without help.
#'
#' @param expr A language object.
#'
#' @return `TRUE` when `expr` contains one of those three calls anywhere.
#'
#' @noRd
has_build_time_call <- function(expr) {
  if (!is_call(expr)) {
    return(FALSE)
  }
  if (is_call(expr, c("after_stat", "stat", "after_scale"))) {
    return(TRUE)
  }
  args <- as.list(expr)[-1]
  length(args) > 0 && any(vapply(args, has_build_time_call, logical(1)))
}

#' Turn a plot's non-symbol positional mappings into fixed columns, on a copy
#'
#' A mapping such as `shuffle(Thumb)` names a different permutation on every
#' render, and independently in every layer that carries it -- there is no
#' seed to declare and no function of the plot's inputs an inferred model
#' could agree with it on. This reads what one evaluation of such a mapping
#' actually drew and rewrites the plot, and every layer that shares the
#' mapping, to point at that fixed vector instead. A model fit from the
#' returned plot's data is then fit on exactly what the returned plot draws.
#'
#' The plot handed in is never modified. A ggplot2 layer is a ggproto object --
#' an environment -- so writing into a layer's own `mapping` field in place
#' would write through to the plot the caller still holds; every layer this
#' touches is replaced by a copy made with `layer_with()`. The plot object itself
#' (`$data`, `$mapping`, `$labels`, `$layers`) is copy-on-modify and safe to
#' assign into directly. `$facet`, `$coordinates` and `$scales` are ggproto
#' too and are never touched here.
#'
#' Only `x` and `y` are pinned by default because those are the two positional
#' aesthetics an inferred model reads. A mapping is left alone -- not pinned,
#' not reported as unreached -- when it is unmapped, when its expression is a
#' bare symbol (there is nothing to evaluate), or when it contains
#' `after_stat()`, `stat()` or `after_scale()` (a build-time instruction, not a
#' value). Everything else is pinned unconditionally: a deterministic mapping
#' such as `log(Height)` is pinned to exactly the numbers it already drew, and
#' the only thing that changes is the mapping's spelling.
#'
#' Every evaluation runs inside ONE `with_random_seed_restored()`, so a caller's
#' RNG stream is exactly where it was before the pin ran -- this is the only
#' place a draw is consumed, and it must not cost a reader a sample they did
#' not ask to spend. Inside that boundary each aesthetic fixes a seed of its
#' own, so one mapping evaluated twice gives one draw while two mappings give
#' two; see the comment on the loop.
#'
#' A plot already carrying a pin for an aesthetic is returned with that
#' aesthetic untouched, and its recorded original preserved: a second
#' `gf_model()` in one pipe must not record `.coursekata_pin_y` as the mapping
#' it is pinning from.
#'
#' @param plot A ggplot object.
#' @param aes Character vector of positional aesthetics to consider.
#' @param call The call to report errors against. Unused today -- this
#'   function never refuses -- kept so a future refusal has somewhere to point.
#'
#' @return A list with `plot` (the pinned copy), `pins` (named list of the
#'   original quosures, empty when nothing was pinned) and `unreached`
#'   (character vector of aesthetics some drawer of which could not be reached).
#'
#' @noRd
pin_plot_values <- function(plot, aes = c("x", "y"), call = caller_env()) {
  spec <- plot_spec(plot)
  pins <- plot_pins(plot)
  unreached <- character(0)

  # ONE save/restore around the whole loop, and a SEPARATE fixed seed per
  # aesthetic inside it. The two are doing different jobs and neither can do
  # the other's.
  #
  # The outer boundary is what keeps the reader's stream where it was: this is
  # the only place a draw is spent, and it must not cost a sample nobody asked
  # to spend.
  #
  # The per-aesthetic seed is what makes one mapping's two evaluations agree.
  # An expression is evaluated once against the plot's data and again against
  # any layer that carries its own copy of it, and the pinned column has to be
  # the SAME permutation both times or the layer draws rows the plot's pin does
  # not describe. A fixed seed gives one draw from two evaluations.
  #
  # The seeds have to differ BETWEEN aesthetics, which is why each is drawn
  # from the stream rather than being a constant: rewinding to one seed for
  # both would hand `shuffle(Thumb) ~ shuffle(Height)` a single permutation
  # applied to each axis, preserving the very relationship a shuffle exists to
  # destroy.
  with_random_seed_restored(for (a in aes) {
    # rule 7: idempotent -- a pin already recorded for this aesthetic is left
    # exactly as it is, original quosure and all
    if (!is.null(pins[[a]])) {
      next
    }

    resolved <- spec$resolve_aes(a)
    if (is.null(resolved) || is.null(resolved$quo)) {
      next
    }

    original <- resolved$quo
    expr <- quo_get_expr(original)
    if (is_symbol(expr) || has_build_time_call(expr)) {
      next
    }

    seed <- sample.int(.Machine$integer.max, 1L)
    v <- with_fixed_seed(seed, eval_tidy(original, resolved$data))
    col <- paste0(".coursekata_pin_", a)
    pinned_quo <- new_quosure(sym(col), base_env())

    for (i in seq_along(plot$layers)) {
      layer <- plot$layers[[i]]
      layer_mapping_a <- layer$mapping[[a]]
      if (is.null(layer_mapping_a)) {
        next # inherits the plot's mapping, which is rewritten below
      }
      if (!identical(layer_mapping_a, original)) {
        # a drawer of `a` with an expression of its own -- the pin cannot
        # reach it, and the caller decides what that means (rule 8)
        unreached <- union(unreached, a)
        next
      }

      new_mapping <- layer$mapping
      new_mapping[[a]] <- pinned_quo
      if (is.data.frame(layer$data)) {
        new_data <- layer$data
        new_data[[col]] <- v
        plot$layers[[i]] <- layer_with(layer, mapping = new_mapping, data = new_data)
      } else {
        # a waiver() layer draws the plot's data; swapping only the mapping
        # is enough because the plot-level write below supplies the column
        plot$layers[[i]] <- layer_with(layer, mapping = new_mapping)
      }
    }

    if (is.data.frame(plot$data)) {
      plot$data[[col]] <- v
    }

    plot$mapping[[a]] <- pinned_quo
    plot$labels[[a]] <- as_label(expr)
    pins[[a]] <- original
  })

  attr(plot, "coursekata_pins") <- pins
  list(plot = plot, pins = pins, unreached = unreached)
}

#' Read the pins `pin_plot_values()` recorded on a plot
#'
#' The accessor for `attr(p, "coursekata_pins")`, so nothing outside this file
#' reaches for the attribute by name.
#'
#' @param p A ggplot object.
#'
#' @return The named list of original quosures recorded by `pin_plot_values()`,
#'   or an empty named list when the plot carries no pin.
#'
#' @noRd
plot_pins <- function(p) {
  attr(p, "coursekata_pins") %||% list()
}
