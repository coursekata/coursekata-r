#' Does an expression compute something only ggplot2's build can supply
#'
#' `after_stat()`, `stat()`, `after_scale()` and `stage()` mark a mapping as a build-time
#' instruction rather than a value: evaluating `after_stat(density)` against a
#' plot's source data does not raise an error, it silently returns the
#' function `stats::density`, which is not a value anyone meant to pin. The
#' expression is walked because the call can be nested inside another one, and
#' `rlang::is_call()` matches a namespace-qualified call
#' (`ggplot2::after_stat(...)`) by function name without help.
#'
#' @param expr A language object.
#'
#' @return `TRUE` when `expr` contains one of those calls anywhere.
#'
#' @noRd
has_build_time_call <- function(expr) {
  if (!is_call(expr)) {
    return(FALSE)
  }
  if (is_call(expr, c("after_stat", "stat", "after_scale", "stage"))) {
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
#' could agree with it on. This pins the selected observation source and other
#' layers that inherit or repeat its expression. Each layer evaluates against
#' its own rows. When a callback constructs those rows, its pin stays on the
#' layer rather than requiring columns in the callback's input frame. A model
#' fit from the resolved source then uses the values that source draws.
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
#' `after_stat()`, `stat()`, `after_scale()` or `stage()` (a build-time instruction, not a
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
#' A reader-supplied axis title (`ylab =`, `labs()`) is preserved rather than
#' overwritten with the pinned mapping's spelling: ggplot2 leaves `$labels`
#' empty when the reader never set one, so the pin's own spelling is written
#' only into that empty slot. `plot_spec(plot)$labels` still reports the
#' original mapping's spelling either way.
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
  plot <- stabilize_source_data(plot)
  spec <- plot_spec(plot)
  plot_rows <- plot$data
  pins <- plot_pins(plot)
  columns <- attr(plot, "coursekata_pin_columns") %||% list()
  provenance <- attr(plot, "coursekata_pin_provenance") %||% list()
  unreached <- character(0)
  # Callback-created columns also belong to the reader. Resolve each drawer
  # once so storage allocation sees them without spending RNG or replaying
  # callback side effects for each aesthetic. The selected source is already
  # resolved by plot_spec(). An unrelated invalid callback is left for build.
  layer_rows <- NULL

  # ONE save/restore around the whole loop, and a SEPARATE fixed seed per
  # aesthetic inside it. The two are doing different jobs and neither can do
  # the other's.
  #
  # The outer boundary is what keeps the reader's stream where it was: this is
  # the only place a draw is spent, and it must not cost a sample nobody asked
  # to spend.
  #
  # The per-aesthetic seed is what makes one mapping's two evaluations agree.
  # An expression is evaluated against the selected source's data and against
  # any other layer's different rows, and the pinned column has to be
  # the SAME permutation both times or the layer draws rows the plot's pin does
  # not describe. A fixed seed gives one draw from two evaluations.
  #
  # The seeds have to differ BETWEEN aesthetics, which is why each is drawn
  # from the stream rather than being a constant: rewinding to one seed for
  # both would hand `shuffle(Thumb) ~ shuffle(Height)` a single permutation
  # applied to each axis, preserving the very relationship a shuffle exists to
  # destroy.
  with_random_seed_restored(for (a in aes) {
    # idempotent: a pin already recorded for this aesthetic is left exactly as
    # it is, original quosure and all
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
    if (is.null(layer_rows)) {
      layer_rows <- lapply(seq_along(plot$layers), function(i) {
        if (identical(i, spec$source$layer_index)) return(spec$data)
        layer <- plot$layers[[i]]
        if (!is.null(attr(layer, "coursekata_layer"))) return(NULL)
        tryCatch(with_random_seed_restored(layer$layer_data(plot_rows)),
                 error = function(e) NULL)
      })
    }
    occupied <- unique(c(names(plot_rows), names(spec$data),
                         unlist(lapply(layer_rows, names)), unlist(columns)))
    col <- utils::tail(
      make.unique(c(occupied, paste0(".coursekata_pin_", a))), 1L
    )
    pinned_quo <- new_quosure(sym(col), base_env())
    plot_owned <- identical(resolved$owner, "plot")
    source <- spec$source$layer
    source_uses_plot_data <- is.null(source) || is.null(source$data) ||
      inherits(source$data, "waiver")
    # Mapping ownership does not imply data ownership. A callback can create
    # the mapped columns, so its pin must not require those columns in the
    # input frame. Keep that pin on its drawers and leave the plot mapping
    # available to future layers. Identical frames can share the existing
    # source evaluation without evaluating the expression on other rows.
    pin_plot_mapping <- plot_owned &&
      (source_uses_plot_data || identical(plot_rows, resolved$data))
    token <- new.env(parent = emptyenv())

    for (i in seq_along(plot$layers)) {
      layer <- plot$layers[[i]]
      if (!is.null(attr(layer, "coursekata_layer"))) {
        # a layer this package added draws its own computed grid, not the
        # plot's observations: neither pinning nor `unreached` applies, and
        # its row count need not match the plot's
        next
      }

      layer_mapping_a <- layer$mapping[[a]]
      inherits_mapping <- is.null(layer_mapping_a)
      if (!plot_owned && inherits_mapping) {
        # A mapping stated only by the source layer does not become a plot-level
        # mapping when pinned. Sibling layers with no mapping did not inherit it
        # before the pin and must not begin inheriting its storage column now.
        next
      }
      if (!inherits_mapping) {
        layer_expr <- if (is_quosure(layer_mapping_a)) {
          quo_get_expr(layer_mapping_a)
        } else {
          layer_mapping_a
        }
        if (!identical(layer_expr, expr) ||
            (is_quosure(layer_mapping_a) &&
              !identical(quo_get_env(layer_mapping_a), quo_get_env(original)))) {
          # a drawer of `a` with an expression different from the plot's -- the
          # pin cannot reach it, and the caller decides what that means
          unreached <- union(unreached, a)
          next
        }
      }

      if (inherits_mapping && !isTRUE(layer$inherit.aes)) {
        # the plot's mapping never reaches this layer at all
        next
      }

      if (is.data.frame(layer$data) || is.function(layer$data)) {
        # Reuse the source evaluation for identical rows. A different frame
        # needs its own evaluation, with the same seed and quosure environment.
        rows <- layer_rows[[i]]
        if (is.null(rows)) {
          unreached <- union(unreached, a)
          next
        }
        lv <- if (identical(rows, resolved$data)) v else {
          try(with_fixed_seed(seed, eval_tidy(original, rows)), silent = TRUE)
        }
        if (inherits(lv, "try-error") || !length(lv) %in% c(1L, nrow(rows))) {
          unreached <- union(unreached, a)
          next
        }
        new_data <- rows
        new_data[[col]] <- lv
        layer_rows[[i]] <- new_data
        if (is.function(layer$data)) {
          new_data <- pin_source_data(layer$data, original, col, seed)
        }
        if (inherits_mapping && pin_plot_mapping) {
          plot$layers[[i]] <- layer_with(layer, data = new_data)
        } else {
          new_mapping <- layer$mapping %||% ggplot2::aes()
          new_mapping[[a]] <- pinned_quo
          plot$layers[[i]] <- layer_with(layer, mapping = new_mapping, data = new_data)
        }
      } else if (!pin_plot_mapping) {
        # This sibling really uses the plot's rows, but the selected source
        # does not. Pin its own evaluation through a live data callback instead
        # of requiring every layer to use one plot-level storage column.
        rows <- layer_rows[[i]]
        if (is.null(rows)) {
          unreached <- union(unreached, a)
          next
        }
        lv <- try(with_fixed_seed(seed, eval_tidy(original, rows)), silent = TRUE)
        if (inherits(lv, "try-error") || !length(lv) %in% c(1L, nrow(rows))) {
          unreached <- union(unreached, a)
          next
        }
        new_mapping <- layer$mapping %||% ggplot2::aes()
        new_mapping[[a]] <- pinned_quo
        plot$layers[[i]] <- layer_with(
          layer, mapping = new_mapping,
          data = pin_source_data(identity, original, col, seed)
        )
        layer_rows[[i]][[col]] <- lv
      } else if (!inherits_mapping) {
        # a waiver() layer draws the plot's data; swapping only the mapping
        # is enough because the plot-level write below supplies the column
        new_mapping <- layer$mapping
        new_mapping[[a]] <- pinned_quo
        plot$layers[[i]] <- layer_with(layer, mapping = new_mapping)
      }
      marked <- layer_with(plot$layers[[i]])
      layer_tokens <- attr(marked, "coursekata_pin_tokens") %||% list()
      layer_tokens[[a]] <- token
      attr(marked, "coursekata_pin_tokens") <- layer_tokens
      # Reading the binding directly avoids ggproto's freshly wrapped method
      # on each `$data` access. Record the final frame/callback after every
      # pin so later pins preserve provenance for earlier aesthetics too.
      attr(marked, "coursekata_pin_data_binding") <- get("data", envir = marked)
      if (isTRUE(attr(layer, "coursekata_source_seed")) &&
          identical(get("data", envir = layer), attr(layer, "coursekata_source_data_binding"))) {
        # Pin wrappers preserve the stabilized callback inside them. Carry
        # that identity forward without blessing an unrelated replacement.
        attr(marked, "coursekata_source_data_binding") <- get("data", envir = marked)
      }
      plot$layers[[i]] <- marked
    }

    if (is.data.frame(plot$data) && pin_plot_mapping) {
      plot$data[[col]] <- if (identical(plot_rows, resolved$data)) v else {
        with_fixed_seed(seed, eval_tidy(original, plot_rows))
      }
      data_tokens <- attr(plot$data, "coursekata_pin_tokens") %||% list()
      data_tokens[[a]] <- token
      attr(plot$data, "coursekata_pin_tokens") <- data_tokens
    }

    if (pin_plot_mapping) {
      plot$mapping[[a]] <- pinned_quo
    }
    # the pin's spelling is a fallback, not an override: ggplot2 derives an
    # axis title from the mapping, which after the rewrite would read
    # `.coursekata_pin_y`. A label already on the plot is one the reader set
    # (`ylab =`, `labs()`); ggplot2 leaves `$labels` empty otherwise, so
    # writing only into the empty slot keeps the reader's words and still
    # never shows the pin column.
    if (is.null(plot$labels[[a]])) {
      plot$labels[[a]] <- as_label(expr)
    }
    pins[[a]] <- original
    columns[[a]] <- col
    provenance[[a]] <- list(token = token, quo = pinned_quo)
  })

  attr(plot, "coursekata_pins") <- pins
  attr(plot, "coursekata_pin_columns") <- columns
  attr(plot, "coursekata_pin_provenance") <- provenance
  list(plot = plot, pins = pins, unreached = unreached)
}

#' Pin a callback's values after it selects or constructs its rows
#' @noRd
pin_source_data <- function(data_fun, original, column, seed) {
  force(data_fun)
  force(original)
  force(column)
  force(seed)
  function(data) {
    rows <- with_random_seed_restored(data_fun(data))
    rows[[column]] <- with_fixed_seed(seed, eval_tidy(original, rows))
    rows
  }
}

#' Read the pins `pin_plot_values()` recorded on a plot
#'
#' The accessor for `attr(p, "coursekata_pins")`, so nothing outside this file
#' reaches for the attribute by name.
#' A pin is valid only while its mapping and data source retain its provenance;
#' an unrelated column with the same spelling must not inherit an old label.
#'
#' @param p A ggplot object.
#'
#' @return The named list of original quosures recorded by `pin_plot_values()`,
#'   or an empty named list when the plot carries no pin.
#'
#' @noRd
plot_pins <- function(p) {
  pins <- attr(p, "coursekata_pins") %||% list()
  columns <- attr(p, "coursekata_pin_columns") %||% list()
  provenance <- attr(p, "coursekata_pin_provenance") %||% list()
  source <- plot_source(p, resolve.data = FALSE)
  mapping <- source$mapping
  pins[vapply(names(pins), function(a) {
    column <- columns[[a]] %||% paste0(".coursekata_pin_", a)
    origin <- provenance[[a]]
    layer <- source$layer
    uses_plot_data <- is.null(layer) || is.null(layer$data) || inherits(layer$data, "waiver")
    source_token <- if (uses_plot_data) {
      (attr(p$data, "coursekata_pin_tokens") %||% list())[[a]]
    } else {
      (attr(layer, "coursekata_pin_tokens") %||% list())[[a]]
    }
    binding_matches <- uses_plot_data || identical(
      get("data", envir = layer), attr(layer, "coursekata_pin_data_binding")
    )
    !is.null(origin) && !is.null(mapping[[a]]) &&
      identical(mapping[[a]], origin$quo) &&
      identical(quo_get_expr(mapping[[a]]), sym(column)) &&
      identical(source_token, origin$token) && binding_matches
  }, logical(1))]
}
