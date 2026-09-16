#' Draw the representation produced by `StatModel`
#'
#' `GeomModel` is a narrow dispatcher. The statistical representation chooses
#' a line, group mark, or intercept; the corresponding ggplot2 geom does the
#' drawing.
#'
#' @format `GeomModel` is a [ggplot2::Geom] object.
#' @export
GeomModel <- ggplot2::ggproto(
  "GeomModel", ggplot2::Geom,
  required_aes = "x|y",
  optional_aes = ".model_kind",
  default_aes = {
    defaults <- ggplot2::GeomLine$default_aes
    defaults$fill <- ggplot2::GeomSmooth$default_aes$fill
    defaults$linewidth <- 1
    defaults$weight <- 1
    defaults
  },
  extra_params = c("na.rm", "orientation", "width", "se"),
  draw_key = ggplot2::GeomLine$draw_key,
  setup_params = function(data, params) {
    params[["flipped_aes"]] <- if (!is.null(params[["orientation"]]) &&
      !is.na(params[["orientation"]])) {
      identical(params[["orientation"]], "y")
    } else if ("flipped_aes" %in% names(data)) {
      isTRUE(data$flipped_aes[[1L]])
    } else {
      FALSE
    }
    params[["width"]] <- params[["width"]] %||% 0.4
    params[["se"]] <- params[["se"]] %||% FALSE
    params
  },
  setup_data = function(data, params) {
    if (nrow(data) == 0L) return(data)
    kinds <- split(seq_len(nrow(data)), data$.model_kind)
    pieces <- lapply(names(kinds), function(kind) {
      piece <- data[kinds[[kind]], , drop = FALSE]
      if (identical(kind, "line")) {
        return(ggplot2::GeomLine$setup_data(piece, params))
      }
      if (identical(kind, "segment")) {
        return(GeomModelMark$setup_data(
          piece,
          list(
            width = params$width,
            mark_axis = if (isTRUE(params$flipped_aes)) "y" else "x"
          )
        ))
      }
      piece
    })
    do.call(rbind, pieces)
  },
  draw_panel = function(data, panel_params, coord, width = 0.4, se = FALSE,
                        flipped_aes = FALSE, na.rm = FALSE) {
    kinds <- split(data, data$.model_kind)
    grobs <- lapply(names(kinds), function(kind) {
      piece <- kinds[[kind]]
      switch(kind,
        line = if (isTRUE(se)) {
          ggplot2::GeomSmooth$draw_panel(
            piece, panel_params, coord, se = TRUE,
            flipped_aes = flipped_aes
          )
        } else {
          ggplot2::GeomLine$draw_panel(piece, panel_params, coord, na.rm = na.rm)
        },
        segment = ggplot2::GeomSegment$draw_panel(
          piece, panel_params, coord, na.rm = na.rm
        ),
        hline = ggplot2::GeomHline$draw_panel(
          piece, panel_params, coord
        ),
        vline = ggplot2::GeomVline$draw_panel(
          piece, panel_params, coord
        ),
        abort(glue("Unknown model geometry `{kind}`."))
      )
    })
    if (length(grobs) == 1L) grobs[[1L]] else do.call(grid::grobTree, grobs)
  }
)
