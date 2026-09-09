#' Draw a data-generating-process frame as a position guide
#'
#' `guide_dgp()` places a truthful null anchor in measured guide space. The
#' population role supplies the population heading, equation, axis line, null
#' label, and triangle. The estimate role supplies the estimate heading,
#' equation, and null label. It never computes a mean or trains a scale.
#'
#' @param value One finite numeric null value in the position scale's data
#'   space.
#' @param role Either `"population"` or `"estimate"`.
#' @param label The null label. A role-specific plotmath label is used by
#'   default.
#' @param equation The model equation. A role-specific plotmath expression is
#'   used by default.
#' @param colour Colour of the keyed null content.
#' @param shape Point shape for the population marker. The default points
#'   toward the panel.
#' @param size Size of the population marker.
#' @param linewidth Width of the population axis line.
#' @param title The role heading. A role-specific heading is used by default.
#' @param theme A theme for this guide, or `NULL`.
#' @param order Guide order.
#' @param position Guide position. The default is derived from the scale.
#'
#' @return A ggplot2 guide.
#'
#' @format `GuideDgp` is a [ggplot2::Guide] object.
#' @export
guide_dgp <- function(value = 0, role = c("population", "estimate"),
                      label = ggplot2::waiver(), equation = ggplot2::waiver(),
                      colour = ggplot2::waiver(), shape = ggplot2::waiver(),
                      size = ggplot2::waiver(), linewidth = ggplot2::waiver(),
                      title = ggplot2::waiver(), theme = NULL, order = 0,
                      position = ggplot2::waiver()) {
  role <- match.arg(role)
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    abort("`guide_dgp()`'s `value` must be one finite number")
  }

  defaults <- dgp_role_defaults(role)
  label <- if (inherits(label, "waiver")) defaults$label else label
  equation <- if (inherits(equation, "waiver")) defaults$equation else equation
  heading <- if (inherits(title, "waiver")) defaults$title else title
  colour <- if (inherits(colour, "waiver")) defaults$colour else colour
  shape <- if (inherits(shape, "waiver") && role == "estimate") NA else shape
  size <- if (inherits(size, "waiver")) defaults$size else size
  linewidth <- if (inherits(linewidth, "waiver")) 0.5 else linewidth

  display <- list(label = label, equation = equation, title = heading)
  for (name in names(display)) {
    if (!is_position_guide_label(display[[name]])) {
      abort(glue("`guide_dgp()`'s `{name}` must be one label or `NULL`"))
    }
  }
  if (!is.character(colour) || length(colour) != 1L || is.na(colour)) {
    abort("`guide_dgp()`'s `colour` must be one colour")
  }
  if (!(inherits(shape, "waiver") || length(shape) == 1L)) {
    abort("`guide_dgp()`'s `shape` must be one point shape")
  }
  if (!is.numeric(size) || length(size) != 1L || !is.finite(size) || size < 0) {
    abort("`guide_dgp()`'s `size` must be one non-negative number")
  }
  if (!is.numeric(linewidth) || length(linewidth) != 1L ||
      !is.finite(linewidth) || linewidth < 0) {
    abort("`guide_dgp()`'s `linewidth` must be one non-negative number")
  }

  ggplot2::new_guide(
    value = value, role = role, label = label, equation = equation,
    heading = heading, colour = colour, shape = shape, size = size,
    linewidth = linewidth, validate_upright = FALSE,
    title = dgp_display_title(heading, equation), theme = theme,
    order = order, position = position, name = "dgp",
    available_aes = c("x", "y"), super = GuideDgp
  )
}

#' @rdname guide_dgp
#' @export
GuideDgp <- ggplot2::ggproto(
  "GuideDgp", ggplot2::GuideAxis,
  params = c(
    ggplot2::GuideAxis$params,
    list(
      value = 0, role = "population", label = NULL, equation = NULL,
      heading = NULL, colour = "#E60000", shape = ggplot2::waiver(),
      size = 4, linewidth = 0.5, validate_upright = FALSE
    )
  ),
  hashables = rlang::exprs(title, key$.value, key$.label, role, name),
  extract_key = function(scale, aesthetic, value, label, role, colour, shape,
                         size, linewidth, ...) {
    anchor <- position_anchor_key(scale, value)
    transformed <- anchor$transformed
    visible <- anchor$visible[[1L]]

    key <- data.frame(if (visible) transformed else NA_real_)
    names(key) <- aesthetic
    key$.value <- value
    key$.label <- I(list(label))
    key$.visible <- visible
    key$role <- role
    key$colour <- colour
    key$shape <- I(list(shape))
    key$size <- size
    key$linewidth <- linewidth
    key
  },
  extract_decor = function(scale, aesthetic, role, ...) {
    if (!identical(role, "population")) return(NULL)
    decor <- data.frame(c(-Inf, Inf))
    names(decor) <- aesthetic
    decor
  },
  transform = function(self, params, coord, panel_params) {
    if (isTRUE(params$validate_upright) &&
        (!inherits(coord, "CoordCartesian") || inherits(coord, "CoordFlip"))) {
      abort(c(
        "`show_dgp()` needs an upright cartesian plot",
        "*" = "its guides describe a horizontal parameter axis above a vertical count axis"
      ))
    }
    ggplot2::GuideAxis$transform(
      params = params, coord = coord, panel_params = panel_params
    )
  },
  setup_elements = function(params, elements, theme) {
    elements <- ggplot2::GuideAxis$setup_elements(params, elements, theme)
    if (identical(params$role, "population") &&
        inherits(elements$line, "element_line")) {
      elements$line$linewidth <- params$linewidth
    }
    elements
  },
  build_ticks = function(key, elements, params) {
    if (!identical(params$role, "population") || !isTRUE(key$.visible[[1L]])) {
      return(grid::nullGrob())
    }
    position_anchor_grob(
      at = key[[params$aes]][[1L]], position = params$position,
      shape = params$shape, size = params$size, colour = params$colour,
      linewidth = params$linewidth
    )
  },
  build_labels = function(key, elements, params) {
    if (!isTRUE(key$.visible[[1L]]) || is.null(params$label)) {
      return(list(grid::nullGrob()))
    }

    at <- key[[params$aes]][[1L]]
    element <- elements$text
    if (inherits(element, "element_blank")) return(list(grid::nullGrob()))
    element$colour <- params$colour
    element$face <- "bold"
    just <- position_anchor_justification(at)

    grob <- if (isTRUE(params$vertical)) {
      ggplot2::element_grob(
        element, label = params$label,
        x = grid::unit(0.5, "npc"), y = grid::unit(at, "npc"),
        vjust = just, margin_x = TRUE, margin_y = TRUE
      )
    } else {
      ggplot2::element_grob(
        element, label = params$label,
        x = grid::unit(at, "npc"), y = grid::unit(0.5, "npc"),
        hjust = just, margin_x = TRUE, margin_y = TRUE
      )
    }
    list(grob)
  },
  measure_grobs = function(grobs, params, elements) {
    rail <- if (identical(params$role, "population") &&
                isTRUE(params$key$.visible[[1L]])) {
      grid::unit(params$size, "mm")
    } else {
      grid::unit(0, "mm")
    }
    spacer <- if (isTRUE(params$key$.visible[[1L]])) {
      grid::unit(0.8, "mm")
    } else {
      grid::unit(0, "mm")
    }
    labels <- grid::unit(params$measure_text(grobs$labels), "cm")
    # The position title is assembled outside the guide gtable. Reserve an
    # outward gutter here so the anchored null label cannot touch the equation
    # in that title, even on the smallest supported device.
    outer <- grid::unit(4, "mm")
    sizes <- grid::unit.c(rail, spacer, labels, outer)
    if (isTRUE(params$lab_first)) sizes <- rev(sizes)
    sizes
  }
)

#' DGP role defaults
#'
#' @param role A DGP guide role.
#'
#' @return A list of display defaults.
#' @noRd
dgp_role_defaults <- function(role) {
  if (identical(role, "population")) {
    list(
      title = "Population Parameter (DGP)",
      equation = expression(Y[i] == beta[0] + beta[1] * X[i] + epsilon[i]),
      label = expression(beta[1] == 0), colour = "#E60000", size = 4
    )
  } else {
    list(
      title = "Parameter Estimate",
      equation = expression(Y[i] == b[0] + b[1] * X[i] + e[i]),
      label = expression(b[1] == 0), colour = "#003d70", size = 4
    )
  }
}

#' Combine a DGP heading and equation into one measured axis title
#'
#' Position-axis titles are drawn once for the complete plot, including under
#' faceting. The keyed null row remains inside `GuideDgp`; using the native axis
#' title for the narrative avoids repeating the heading over every panel.
#'
#' @param heading,equation Optional one-item labels.
#'
#' @return A character value, expression, or `NULL`.
#' @noRd
dgp_display_title <- function(heading, equation) {
  if (is.null(heading)) return(equation)
  if (is.null(equation)) return(heading)
  heading <- if (is.expression(heading)) heading[[1L]] else heading
  equation <- if (is.expression(equation)) equation[[1L]] else equation
  as.expression(call("atop", call("bold", heading), call("bold", equation)))
}

#' Mark a DGP guide for the high-level upright-only teaching composition
#'
#' @param guide A `GuideDgp` instance.
#'
#' @return A detached guide with build-time coordinate validation enabled.
#' @noRd
dgp_upright_guide <- function(guide) {
  guide <- clone_position_guide(guide)
  guide$params$validate_upright <- TRUE
  guide
}

#' Preserve the teaching helper's null-label typography inside its guide
#'
#' @return A partial ggplot2 theme.
#' @noRd
dgp_null_theme <- function() {
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 5 * ggplot2::.pt, face = "bold"),
    axis.text.y = ggplot2::element_text(size = 5 * ggplot2::.pt, face = "bold")
  )
}
