#' Keep annotation computations on their own build's trained scales
#'
#' ggplot2 clones position scales for each build. Attaching our own metadata to
#' that clone scopes it to both panel and build, including recursive builds.
#' No plot-owned scale is changed, and failed builds need no stack cleanup.
#' @noRd
b_panel_frame <- function(scales, key, panel, value = NULL) {
  scale <- scales$x %||% scales$y
  frames <- attr(scale, "coursekata_b_frames") %||% list()
  hit <- which(vapply(frames, function(frame) {
    identical(frame$key, key) && identical(frame$panel, panel)
  }, logical(1)))
  if (!is.null(value)) {
    frame <- new.env(parent = emptyenv())
    frame$key <- key
    frame$panel <- panel
    frame$rows <- value
    if (length(hit)) frames[[hit[[1L]]]] <- frame else frames <- c(frames, list(frame))
    attr(scale, "coursekata_b_frames") <- frames
    return(frame)
  }
  if (length(hit)) frames[[hit[[1L]]]] else NULL
}

#' Observe a model result or a source's mapped observations
#' @noRd
b_observer_stat <- function(parent, state, model = FALSE) {
  if (inherits(parent, "StatIdentity")) {
    parent <- ggplot2::ggproto("StatIdentity", ggplot2::Stat,
      compute_panel = function(data, scales) data)
  }
  ggplot2::ggproto(NULL, parent,
    setup_params = function(data, params) {
      if (state$inferred && length(unique(data$PANEL)) > 1L) {
        abort(glue("`{state$fn}()` needs a fitted model on a faceted plot."), call = state$call)
      }
      parent$setup_params(data, params)
    },
    compute_panel = function(data, scales, ...) {
      panel <- as.character(data$PANEL[[1L]])
      if (!model) b_panel_frame(scales, state, panel, data)
      result <- parent$compute_panel(data, scales, ...)
      if (model) b_panel_frame(scales, state, panel, result)
      result
    }
  )
}

#' Run the canonical model stat on the observations already mapped by ggplot2
#'
#' The source stat has recorded its input before aggregation or position
#' adjustment. Reading those rows avoids a second data callback or a second
#' evaluation of stochastic mappings, and follows mappings added after geom_b.
#' @noRd
StatBModel <- ggplot2::ggproto("StatBModel", ggplot2::Stat,
  required_aes = character(),
  compute_panel = function(data, scales, state, fn, call) {
    panel <- as.character(data$PANEL[[1L]])
    frame <- b_panel_frame(scales, state, panel)
    rows <- frame$rows
    frame$failed <- TRUE
    if (is.null(rows) || !nrow(rows)) return(data.frame())
    columns <- intersect(c("x", "y", "weight", "group", "PANEL", ".b_order_x", ".b_order_y"), names(rows))
    rows <- rows[columns]
    axes <- intersect(c("x", "y"), names(rows))
    # We enter the canonical stat at its panel hook, after our source's stat.
    # Apply the same required-position filtering as Stat$compute_layer first.
    rows <- ggplot2::remove_missing(rows, na.rm = TRUE,
      vars = intersect(c(axes, StatModel$non_missing_aes), names(rows)), finite = TRUE)
    if (!nrow(rows)) return(data.frame())
    discrete <- axes[vapply(rows[axes], inherits, logical(1), "mapped_discrete")]
    if (length(discrete) == length(axes)) {
      abort(glue("`{fn}()` needs a numeric outcome axis."), call = call)
    }
    flipped <- identical(axes, "x") || identical(discrete, "y")
    if (length(discrete)) {
      axis <- discrete[[1L]]
      reference_order <- rows[[paste0(".b_order_", axis)]] %||% rows$group
      positions <- unique(rows[[axis]][order(reference_order)])
      rows$group <- match(rows[[axis]], positions)
    } else rows$group <- 1L
    params <- StatModel$setup_params(rows, list(orientation = if (flipped) "y" else "x"))
    params$orientation <- NULL
    result <- do.call(StatModel$compute_panel, c(list(data = rows, scales = scales), params))
    b_panel_frame(scales, state, panel, result)
    data.frame()
  }
)

#' Project one component from the final model grid or supplied coefficients
#' @noRd
StatBMark <- ggplot2::ggproto("StatBMark", ggplot2::Stat,
  required_aes = character(),
  compute_panel = function(data, scales, state, spec, role) {
    panel <- as.character(data$PANEL[[1L]])
    frame <- b_panel_frame(scales, state, panel)
    if (is.null(frame) || isTRUE(frame$failed)) return(data.frame())
    if (is.null(frame$marks)) {
      frame$failed <- TRUE
      rows <- frame$rows
      args <- spec$args
      if (is.null(spec$model)) {
        claim <- b_grid_coefficients(rows, NULL, scales, spec$fn, spec$call)
        coefs <- claim$coefs
        values <- claim$values
        args$positions <- claim$positions
        outcome <- claim$outcome_axis
        predictor <- claim$predictor
        categorical <- claim$categorical
      } else {
        coefs <- spec$coefs
        outcome_column <- paste0(".b_outcome_axis_", names(spec$model$model)[[1L]])
        outcome <- unique(rows[[outcome_column]]) %||% spec$outcome_axis
        predictor <- spec$predictor
        categorical <- spec$categorical
        axis <- setdiff(c("x", "y"), outcome)
        scale <- scales[[axis]]
        values <- rows[[axis]]
        if (isTRUE(categorical)) {
          args$positions <- as.numeric(scale$map(spec$model$xlevels[[predictor]]))
        } else if (!is.null(predictor)) {
          values <- scale$get_transformation()$inverse(values[is.finite(values)])
          if (!length(values) || diff(range(values)) == 0) values <- spec$model$model[[predictor]]
        }
      }
      plan <- b_plan(outcome, predictor, categorical, values, coefs, args)
      if (!is.null(spec$model)) plan$marks <- b_transform_marks(plan$marks, scales)
      marks <- Filter(function(mark) !is.null(attr(mark, "coursekata_layer")), plan$marks)
      names(marks) <- vapply(marks, function(mark) attr(mark, "coursekata_layer"), character(1))
      frame$marks <- lapply(marks, function(mark) {
        data <- mark$data
        data$.b_type <- if (inherits(mark$geom, "GeomPoint")) "point" else "segment"
        for (name in names(mark$aes_params)) data[[name]] <- mark$aes_params[[name]]
        if (inherits(mark$geom, "GeomPoint") && is.null(mark$aes_params$alpha)) {
          data$alpha <- ggplot2::get_geom_defaults("point")$alpha
        }
        if (!is.null(mark$geom_params$x_just)) {
          data$.b_x_just <- mark$geom_params$x_just
          data$.b_y_just <- mark$geom_params$y_just
        }
        data
      })
      frame$failed <- FALSE
    }
    marks <- frame$marks
    if (role %in% c("bk_more", "bk_more_label")) {
      pattern <- if (role == "bk_more") "^bk_[0-9]+$" else "^bk_[0-9]+_label$"
      tags <- grep(pattern, names(marks), value = TRUE)
      numbers <- as.integer(sub("^bk_([0-9]+).*", "\\1", tags))
      return(do.call(rbind, marks[tags[numbers > spec$reserved]]) %||% data.frame())
    }
    marks[[role]] %||% data.frame()
  }
)

#' Draw the intercept as the point or reference segment selected by the stat
#' @noRd
GeomBIntercept <- ggplot2::ggproto("GeomBIntercept", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(colour = "black", linewidth = 0.8, size = 4,
                            alpha = 1, shape = 21, fill = "white", stroke = 1,
                            linetype = 1),
  draw_key = ggplot2::draw_key_blank,
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    geom <- if (identical(data$.b_type[[1L]], "point")) ggplot2::GeomPoint else ggplot2::GeomSegment
    geom$draw_panel(data, panel_params, coord, na.rm = na.rm)
  }
)

#' Evaluate a source's final mappings reproducibly without evaluating them early
#'
#' The public layer hook runs after inheritance is resolved, so later aes()
#' additions remain authoritative. ggproto_parent preserves the layer's own
#' aesthetic computation and grouping. The caller's random stream is restored.
#' @noRd
b_seeded_source <- function(source, fn, call, check_coding = FALSE, model = NULL) {
  parent <- source
  seed <- with_random_seed_restored(sample.int(.Machine$integer.max, 2L)[[2L]])
  layer_with(source, compute_aesthetics = function(self, data, plot) {
    result <- with_fixed_seed(seed, ggplot2::ggproto_parent(parent, self)$compute_aesthetics(data, plot))
    if (!is.null(model)) {
      mapping <- attr(self, "coursekata_b_source_mapping") %||% self$computed_mapping
      axes <- mapping[intersect(c("x", "y"), names(mapping))]
      axes <- axes[!vapply(axes, function(q) has_build_time_call(quo_get_expr(q)), logical(1))]
      labels <- vapply(axes, source_mapping_label, character(1))
      pins <- plot_pins(plot)
      canonical <- plot_source(plot, resolve.data = FALSE)$mapping
      for (axis in intersect(names(labels), names(pins))) {
        if (identical(mapping[[axis]], canonical[[axis]])) {
          labels[[axis]] <- source_mapping_label(pins[[axis]])
        }
      }
      outcome <- names(model$model)[[1L]]
      outcome_axis <- names(labels)[labels == outcome]
      if (length(outcome_axis) != 1L) {
        abort(glue("`{fn}()` needs the axis carrying the model's outcome `{outcome}`."), call = call)
      }
      predictors <- setdiff(names(model$model)[-1L], "(weights)")
      check_b_predictor(list(axes = labels), outcome_axis,
                        if (length(predictors)) predictors else NULL, fn, call)
      result[[paste0(".b_outcome_axis_", outcome)]] <- outcome_axis
    }
    for (axis in intersect(c("x", "y"), names(result))) {
      if (!is.numeric(result[[axis]])) {
        result[[paste0(".b_order_", axis)]] <- as.integer(factor(result[[axis]]))
      }
    }
    if (check_coding) {
      for (axis in intersect(c("x", "y"), names(result))) {
        values <- result[[axis]]
        if (!is.numeric(values)) {
          coding <- attr(values, "contrasts") %||%
            getOption("contrasts")[[if (is.ordered(values)) 2L else 1L]]
          check_b_coding(coding, axis, fn, call)
        }
      }
    }
    result
  })
}

#' Assemble facet-neutral role layers without reading data or building a plot
#' @noRd
b_defer_spec <- function(spec) {
  state <- new.env(parent = emptyenv())
  state$inferred <- is.null(spec$model)
  state$fn <- spec$fn
  state$call <- spec$call
  source_index <- spec$source$layer_index
  existing <- is.null(spec$model) && length(spec$model_index) == 1L
  if (existing) {
    index <- spec$model_index
    source <- spec$plot$layers[[index]]
    source <- b_seeded_source(source, spec$fn, spec$call, check_coding = TRUE)
    spec$plot$layers[[index]] <- layer_with(source,
      stat = b_observer_stat(source$stat, state, model = TRUE))
  } else {
    if (is.na(source_index)) {
      source <- ggplot2::layer(geom = ggplot2::GeomBlank,
        stat = b_observer_stat(ggplot2::StatIdentity, state), position = "identity",
        data = NULL, mapping = NULL, inherit.aes = TRUE)
      source <- b_seeded_source(source, spec$fn, spec$call,
        check_coding = is.null(spec$model), model = spec$model)
      spec$plot <- spec$plot + tag_layer(source, "b_source")
    } else {
      source <- spec$plot$layers[[source_index]]
      source <- b_seeded_source(source, spec$fn, spec$call,
                                check_coding = is.null(spec$model), model = spec$model)
      spec$plot$layers[[source_index]] <- layer_with(source,
        stat = b_observer_stat(source$stat, state))
    }
    if (is.null(spec$model)) {
      model <- ggplot2::layer(geom = ggplot2::GeomBlank, stat = StatBModel,
        position = "identity", data = data.frame(.b_placeholder = 1),
        mapping = ggplot2::aes(), inherit.aes = FALSE, show.legend = FALSE,
        params = list(state = state, fn = spec$fn, call = spec$call))
      spec$plot <- spec$plot + tag_layer(model, "b_model_source")
    }
  }
  # Keep familiar numbered roles for a supplied fit and the first two
  # differences of an inferred fit. Further categories share variable-row
  # segment and label layers, so replacing data never freezes cardinality.
  reserved <- if (isTRUE(spec$categorical)) length(spec$coefs) else 3L
  args <- spec$args
  args["run"] <- list(NULL)
  args$positions <- seq_len(max(2L, reserved))
  args$show_b0 <- TRUE
  categorical <- b_plan("y", "x", TRUE, NULL, rep(0, max(2L, reserved)), args)$marks
  continuous <- b_plan("y", "x", FALSE, c(0, 10), c(0, 1), args)$marks
  marks <- continuous
  tags <- vapply(marks, function(mark) attr(mark, "coursekata_layer"), character(1))
  marks <- c(marks, Filter(function(mark) !attr(mark, "coursekata_layer") %in% tags, categorical))
  for (suffix in c("", "_label")) {
    template <- categorical[[which(vapply(categorical, function(mark) {
      identical(attr(mark, "coursekata_layer"), paste0("bk_2", suffix))
    }, logical(1)))]]
    marks <- c(marks, list(tag_layer(layer_with(template), paste0("bk_more", suffix))))
  }
  claim <- spec[c("args", "fn", "call", "outcome_axis", "predictor", "categorical", "coefs", "model")]
  claim$reserved <- reserved
  if (!isTRUE(spec$args$show_b0)) {
    marks <- Filter(function(mark) !attr(mark, "coursekata_layer") %in% c("b0", "b0_label"), marks)
  }
  spec$marks <- lapply(marks, function(mark) {
    role <- attr(mark, "coursekata_layer")
    params <- mark$geom_params
    if (inherits(mark$geom, "GeomBText")) params$fn <- spec$fn
    geom <- if (role == "b0") GeomBIntercept else mark$geom
    # Styling and coordinates come from the build-time plan. Placeholder rows
    # deliberately have no x/y fields: either name may be a facet variable.
    layer_with(mark, stat = StatBMark, geom = geom,
      data = data.frame(.b_placeholder = 1), mapping = ggplot2::aes(),
      aes_params = list(), geom_params = params,
      stat_params = list(na.rm = TRUE, state = state, spec = claim, role = role))
  })
  spec
}
