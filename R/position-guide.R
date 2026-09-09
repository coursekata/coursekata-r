#' Copy a plot before changing one of its position-scale guides
#'
#' ggplot2 scales and guide collections are ggproto objects. Copying only the
#' plot would therefore let a guide edit leak back into the plot the caller
#' still holds. This helper clones the scale collection, materializes an
#' ordinary continuous position scale when none is explicit, and copies the
#' guide collection before removing an override that is moved onto the scale.
#'
#' @param plot A ggplot object.
#' @param aesthetic The mapped position aesthetic, `"x"` or `"y"`.
#'
#' @return A list containing the copied plot, copied or materialized scale,
#'   effective guide, effective physical aesthetic, and whether the guide was
#'   supplied as a plot-level override.
#' @noRd
position_guide_state <- function(plot, aesthetic = "x") {
  stopifnot(aesthetic %in% c("x", "y"))

  out <- plot
  out$scales <- plot$scales$clone()
  scale <- out$scales$get_scales(aesthetic)
  if (is.null(scale)) {
    scale <- switch(aesthetic,
      x = ggplot2::scale_x_continuous(),
      y = ggplot2::scale_y_continuous()
    )
    out$scales$add(scale)
  }

  physical <- if (inherits(out$coordinates, "CoordFlip")) {
    switch(aesthetic, x = "y", y = "x")
  } else {
    aesthetic
  }
  overrides <- out$guides$guides
  from_override <- physical %in% names(overrides)
  guide <- if (from_override) overrides[[physical]] else scale$guide

  if (from_override) {
    overrides[physical] <- NULL
    out$guides <- ggplot2::ggproto(NULL, out$guides, guides = overrides)
  }

  list(
    plot = out, scale = scale, guide = guide, physical = physical,
    from_override = from_override
  )
}

#' Detach a guide ggproto object from the caller's plot
#'
#' @param guide A guide object or character guide specification.
#'
#' @return A shallow ggproto copy, or the original non-ggproto value.
#' @noRd
clone_position_guide <- function(guide) {
  if (!inherits(guide, "Guide")) return(guide)
  ggplot2::ggproto(NULL, guide, params = guide$params)
}

#' Whether a guide value means that no guide should be drawn
#'
#' @param guide A guide object, name, or sentinel.
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
guide_is_suppressed <- function(guide) {
  is.null(guide) || identical(guide, FALSE) || identical(guide, "none") ||
    inherits(guide, "GuideNone")
}

#' Read the children from an axis guide or axis stack
#'
#' A waiver is ggplot2's implicit ordinary axis. Suppression contributes no
#' caller child. An existing stack is flattened so repeated helper calls rebuild
#' one stack instead of nesting stacks or duplicating the numeric axis.
#'
#' @param guide A resolved position guide.
#'
#' @return A list of guide objects or guide names.
#' @noRd
position_guide_children <- function(guide) {
  if (inherits(guide, "waiver")) {
    return(list(ggplot2::guide_axis()))
  }
  if (guide_is_suppressed(guide)) {
    return(list())
  }
  if (inherits(guide, "GuideAxisStack")) {
    return(lapply(guide$params$guides, clone_position_guide))
  }
  list(clone_position_guide(guide))
}

#' Whether a guide belongs to CourseKata's position annotation family
#'
#' @param guide A guide object.
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
is_coursekata_position_guide <- function(guide) {
  inherits(guide, "GuideCutoff") || inherits(guide, "GuideDgp")
}

#' Put CourseKata guide children in their teaching order
#'
#' Caller guides retain their relative order and stay nearest the panel.
#' Cutoff calls follow in `call_id` order, followed by the estimate DGP guide.
#' Population DGP content is installed on the secondary position and is never
#' part of this primary stack.
#'
#' @param guides A list of guide children.
#'
#' @return The reordered list.
#' @noRd
order_coursekata_position_guides <- function(guides) {
  caller <- Filter(function(x) !is_coursekata_position_guide(x), guides)
  cutoff <- Filter(function(x) inherits(x, "GuideCutoff"), guides)
  dgp <- Filter(
    function(x) inherits(x, "GuideDgp") && identical(x$params$role, "estimate"),
    guides
  )

  if (length(cutoff) > 1L) {
    ids <- vapply(cutoff, function(x) x$params$call_id %||% 1L, integer(1))
    cutoff <- cutoff[order(ids, seq_along(ids))]
  }
  c(caller, cutoff, dgp)
}

#' Rebuild one flat axis stack
#'
#' @param children The guide children, already ordered.
#' @param template The resolved guide before the new child was added.
#'
#' @return A `GuideAxisStack`.
#' @noRd
new_position_guide_stack <- function(children, template) {
  if (length(children) == 0L) {
    return("none")
  }

  stack_params <- if (inherits(template, "GuideAxisStack")) template$params else NULL
  if (is.null(stack_params)) {
    position <- if (length(children) > 0L && inherits(children[[1L]], "Guide")) {
      children[[1L]]$params$position
    } else {
      ggplot2::waiver()
    }
  } else {
    position <- stack_params$position
  }
  if (is.null(position)) position <- ggplot2::waiver()

  estimate <- Filter(
    function(x) inherits(x, "GuideDgp") && identical(x$params$role, "estimate"),
    children
  )
  title <- if (length(estimate) > 0L) {
    estimate[[length(estimate)]]$params$title
  } else {
    stack_params$title %||% ggplot2::waiver()
  }

  args <- c(
    list(first = children[[1L]]),
    unname(children[-1L]),
    list(
      title = title,
      theme = stack_params$theme %||% NULL,
      spacing = stack_params$spacing %||% NULL,
      order = stack_params$order %||% 0,
      position = position
    )
  )
  stack <- do.call(ggplot2::guide_axis_stack, args)
  if (!is.null(stack_params$angle)) stack$params$angle <- stack_params$angle
  if (!is.null(stack_params$direction)) stack$params$direction <- stack_params$direction
  stack
}

#' Add one CourseKata child to a mapped position scale
#'
#' @param plot A ggplot object.
#' @param guide A `GuideDgp` or `GuideCutoff` instance.
#' @param aesthetic The mapped position aesthetic.
#'
#' @return A copied ggplot with one rebuilt scale-owned guide stack.
#' @noRd
add_position_guide_child <- function(plot, guide, aesthetic = "x") {
  state <- position_guide_state(plot, aesthetic)
  children <- position_guide_children(state$guide)
  children <- order_coursekata_position_guides(c(children, list(guide)))
  state$scale$guide <- new_position_guide_stack(children, state$guide)
  state$plot
}

#' Find CourseKata guide children recursively
#'
#' @param guide A guide object, guide name, or secondary-axis object.
#' @param class A ggproto class name.
#' @param role Optional DGP role.
#'
#' @return A list of matching guide objects.
#' @noRd
position_guide_matches <- function(guide, class, role = NULL) {
  if (inherits(guide, "GuideAxisStack")) {
    return(unlist(
      lapply(guide$params$guides, position_guide_matches, class = class, role = role),
      recursive = FALSE
    ))
  }
  if (inherits(guide, "AxisSecondary")) {
    return(position_guide_matches(guide$guide, class = class, role = role))
  }
  if (!inherits(guide, class)) return(list())
  if (!is.null(role) && !identical(guide$params$role, role)) return(list())
  list(guide)
}

#' Resolve a position guide's requested side
#'
#' @param guide A resolved guide.
#' @param scale_position The scale's default side.
#'
#' @return One of `"top"`, `"bottom"`, `"left"`, or `"right"`.
#' @noRd
resolved_position_guide_side <- function(guide, scale_position) {
  position <- if (inherits(guide, "Guide")) guide$params$position else NULL
  if (is.null(position) || inherits(position, "waiver")) scale_position else position
}

#' Install the two scale-owned guides used by `show_dgp()`
#'
#' @param plot A ggplot object.
#' @param estimate,population `GuideDgp` instances.
#' @param call The call to report for semantic conflicts.
#'
#' @return A copied ggplot object.
#' @noRd
add_dgp_position_guides <- function(plot, estimate, population,
                                    call = caller_env()) {
  state <- position_guide_state(plot, "x")
  side <- resolved_position_guide_side(state$guide, state$scale$position)
  if (!identical(side, "bottom")) {
    abort(
      c(
        "`show_dgp()` needs the primary x guide at the bottom",
        "i" = "A top primary guide reverses the population and estimate frame."
      ),
      call = call
    )
  }

  secondary_override <- state$plot$guides$guides[["x.sec"]]
  secondary <- state$scale$secondary.axis
  if (!is.null(secondary_override) || !inherits(secondary, "waiver")) {
    abort(
      c(
        "`show_dgp()` needs the secondary x position for the population frame",
        "i" = "This plot already supplies a secondary x axis or guide."
      ),
      call = call
    )
  }

  children <- position_guide_children(state$guide)
  children <- order_coursekata_position_guides(c(children, list(estimate)))
  state$scale$guide <- new_position_guide_stack(children, state$guide)
  state$scale$secondary.axis <- ggplot2::dup_axis(guide = population)
  state$plot
}
