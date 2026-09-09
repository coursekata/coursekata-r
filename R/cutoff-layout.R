.cutoff_layout_defaults <- list(
  box_panel_margin = 2,
  route_clearance = 5,
  label_gap = 2,
  candidate_grid_min = 5L,
  candidate_grid_max = 9L,
  candidate_grid_offset = 4L,
  depth_cost = 0.45,
  outward_cost = 30,
  global_candidates = 12L,
  per_axis_candidates = 4L,
  max_candidates = 48L,
  avoidance_samples = 7L,
  avoidance_mean_cost = 220,
  avoidance_max_cost = 180,
  box_gap = 1.5,
  route_panel_margin = 1,
  route_gaps = c(1.5, 3, 5),
  route_min_room = 0.5,
  route_tolerance = 0.2,
  route_track_limit = 10L,
  route_detour_offsets = c(2, 5),
  route_detour_limit = 8L,
  segment_tolerance = 0.15,
  shared_track_cost = 1000,
  crossing_cost = 120,
  box_hit_cost = 10000,
  route_length_cost = 0.02,
  beam_width = 3L,
  overlap_cost = 1e6
)

cutoff_physical_sides <- function(data) {
  call_id <- if ("call_id" %in% names(data)) data$call_id else seq_len(nrow(data))
  semantic <- if ("side" %in% names(data)) data$side else NA_character_
  physical <- rep(NA_character_, nrow(data))
  groups <- split(seq_len(nrow(data)), call_id)

  for (indices in groups) {
    if (length(indices) == 1L) {
      physical[indices] <- if (data$.screen[indices] < 0.5) "lower" else "upper"
      next
    }
    order_on_screen <- order(
      data$.screen[indices],
      match(semantic[indices], c("lower", "upper")),
      na.last = TRUE
    )
    ordered <- indices[order_on_screen]
    physical[ordered[[1L]]] <- "lower"
    physical[ordered[[length(ordered)]]] <- "upper"
  }
  physical[is.na(physical)] <- semantic[is.na(physical)]
  physical[is.na(physical)] <- ifelse(
    data$.screen[is.na(physical)] < 0.5, "lower", "upper"
  )
  physical
}

# Prepare the two occupancy interpolators once for the whole panel. Candidate
# scoring then samples the prepared profile without rebuilding it for each box.
prepare_cutoff_avoidance <- function(avoidance, horizontal, panel_width,
                                     panel_height) {
  if (is.null(avoidance) || nrow(avoidance) < 2L) return(NULL)
  axis_extent <- if (horizontal) panel_height else panel_width
  depth_extent <- if (horizontal) panel_width else panel_height
  axis <- avoidance$axis * axis_extent
  ordered <- order(axis)
  list(
    baseline = stats::approxfun(
      axis[ordered], avoidance$baseline[ordered] * depth_extent,
      rule = 2, ties = "ordered"
    ),
    tip = stats::approxfun(
      axis[ordered], avoidance$tip[ordered] * depth_extent,
      rule = 2, ties = "ordered"
    )
  )
}

#' Generate nearby box positions for one cutoff label
#'
#' The preferred position is just beyond the stem and outward into the tail of
#' the distribution. Further depth rows and modest axis shifts are fallbacks;
#' they are candidates, not fixed lanes.
#'
#' @param row One transformed cutoff row.
#' @param box_width,box_height Measured box dimensions in millimetres.
#' @param side Physical screen side of the callout.
#' @param horizontal Whether the stem runs horizontally.
#' @param height Stem height as a panel fraction.
#' @param panel_width,panel_height Panel dimensions in millimetres.
#' @param count Total number of labels being placed.
#' @param avoidance Prepared distribution occupancy profile.
#'
#' @return A list of candidate rectangles and their preference costs.
#' @noRd
cutoff_box_candidates <- function(row, box_width, box_height, side,
                                  horizontal, height, panel_width,
                                  panel_height, count, avoidance = NULL) {
  defaults <- .cutoff_layout_defaults
  outward <- if (identical(side, "lower")) -1 else 1

  boundary_x <- row$x * panel_width
  boundary_y <- row$y * panel_height
  if (horizontal) {
    inward <- sign(row$.opposite_x - row$x)
    if (!is.finite(inward) || inward == 0) inward <- 1
    source_depth <- boundary_x + inward * min(
      height * panel_width,
      abs(row$.opposite_x - row$x) * panel_width
    )
    axis_anchor <- boundary_y
    axis_extent <- panel_height
    axis_size <- box_height
    depth_extent <- panel_width
    depth_size <- box_width
  } else {
    inward <- sign(row$.opposite_y - row$y)
    if (!is.finite(inward) || inward == 0) inward <- 1
    source_depth <- boundary_y + inward * min(
      height * panel_height,
      abs(row$.opposite_y - row$y) * panel_height
    )
    axis_anchor <- boundary_x
    axis_extent <- panel_width
    axis_size <- box_width
    depth_extent <- panel_height
    depth_size <- box_height
  }

  axis_min <- defaults$box_panel_margin + axis_size / 2
  axis_max <- axis_extent - defaults$box_panel_margin - axis_size / 2
  if (outward < 0) axis_min <- axis_min + defaults$route_clearance
  if (outward > 0) axis_max <- axis_max - defaults$route_clearance
  if (axis_min > axis_max) {
    axis_min <- axis_max <- axis_extent / 2
  }

  preferred_axis <- axis_anchor + outward *
    (axis_size / 2 + defaults$label_gap)
  preferred_axis <- min(axis_max, max(axis_min, preferred_axis))
  grid_count <- max(
    defaults$candidate_grid_min,
    min(
      defaults$candidate_grid_max,
      count + defaults$candidate_grid_offset
    )
  )
  axis_grid <- seq(axis_min, axis_max, length.out = grid_count)
  axis_values <- c(
    preferred_axis, axis_anchor,
    preferred_axis - outward * (axis_size + defaults$label_gap),
    if (outward < 0) axis_min else axis_max,
    axis_grid
  )
  axis_values <- pmin(axis_max, pmax(axis_min, axis_values))
  axis_values <- unique(round(axis_values, 3))

  preferred_depth <- source_depth + inward *
    (depth_size / 2 + defaults$label_gap)
  depth_min <- defaults$box_panel_margin + depth_size / 2
  depth_max <- depth_extent - defaults$box_panel_margin - depth_size / 2
  if (depth_min > depth_max) {
    depth_min <- depth_max <- depth_extent / 2
  }
  preferred_depth <- min(depth_max, max(depth_min, preferred_depth))
  far_depth <- if (inward > 0) depth_max else depth_min
  depth_values <- seq(preferred_depth, far_depth, length.out = grid_count)
  depth_values <- unique(round(depth_values, 3))

  candidates <- vector("list", length(axis_values) * length(depth_values))
  at <- 0L
  for (axis in axis_values) {
    for (depth in depth_values) {
      at <- at + 1L
      if (horizontal) {
        centre_x <- depth
        centre_y <- axis
      } else {
        centre_x <- axis
        centre_y <- depth
      }
      outward_departure <- max(0, -outward * (axis - axis_anchor))
      candidates[[at]] <- list(
        x1 = centre_x - box_width / 2,
        x2 = centre_x + box_width / 2,
        y1 = centre_y - box_height / 2,
        y2 = centre_y + box_height / 2,
        x = centre_x, y = centre_y, axis = axis, depth = depth,
        cost = abs(axis - preferred_axis) +
          defaults$depth_cost * abs(depth - preferred_depth) +
          defaults$outward_cost * outward_departure
      )
      candidates[[at]]$cost <- candidates[[at]]$cost +
        cutoff_box_avoidance_cost(candidates[[at]], horizontal, avoidance)
    }
  }
  costs <- vapply(candidates, `[[`, numeric(1), "cost")
  global <- candidates[order(costs)[seq_len(min(
    defaults$global_candidates, length(costs)
  ))]]
  by_axis <- split(
    candidates,
    vapply(candidates, function(candidate) candidate$axis, numeric(1))
  )
  diverse <- unlist(lapply(by_axis, function(group) {
    group_costs <- vapply(group, `[[`, numeric(1), "cost")
    group[order(group_costs)[seq_len(min(
      defaults$per_axis_candidates, length(group_costs)
    ))]]
  }), recursive = FALSE)
  candidates <- c(global, diverse)
  keys <- vapply(candidates, function(candidate) {
    paste(round(candidate$x, 3), round(candidate$y, 3), sep = ":")
  }, character(1))
  candidates <- candidates[!duplicated(keys)]
  costs <- vapply(candidates, `[[`, numeric(1), "cost")
  candidates[order(costs)[seq_len(min(
    defaults$max_candidates, length(costs)
  ))]]
}

cutoff_box_avoidance_cost <- function(box, horizontal, avoidance) {
  if (is.null(avoidance)) return(0)
  defaults <- .cutoff_layout_defaults
  axis_range <- if (horizontal) c(box$y1, box$y2) else c(box$x1, box$x2)
  depth_range <- if (horizontal) c(box$x1, box$x2) else c(box$y1, box$y2)
  sample_axis <- seq(
    axis_range[[1L]], axis_range[[2L]],
    length.out = defaults$avoidance_samples
  )
  baseline <- avoidance$baseline(sample_axis)
  tip <- avoidance$tip(sample_axis)
  occupied_low <- pmin(baseline, tip)
  occupied_high <- pmax(baseline, tip)
  overlap <- pmax(
    0,
    pmin(depth_range[[2L]], occupied_high) -
      pmax(depth_range[[1L]], occupied_low)
  )
  fraction <- overlap / max(0.1, diff(depth_range))
  defaults$avoidance_mean_cost * mean(fraction) +
    defaults$avoidance_max_cost * max(fraction)
}

cutoff_boxes_overlap <- function(first, second,
                                 gap = .cutoff_layout_defaults$box_gap) {
  first$x1 < second$x2 + gap && first$x2 > second$x1 - gap &&
    first$y1 < second$y2 + gap && first$y2 > second$y1 - gap
}

cutoff_nearest_box_port <- function(source, box) {
  ports <- rbind(
    right = c(box$x2, box$y),
    top = c(box$x, box$y2),
    left = c(box$x1, box$y),
    bottom = c(box$x, box$y1)
  )
  angles <- c(right = 0, top = 90, left = 180, bottom = 270)
  closest <- which.min(rowSums((ports - rep(source, each = 4L))^2))
  list(point = unname(ports[closest, ]), angle = unname(angles[[closest]]))
}

cutoff_nearby_route_positions <- function(values, anchor, limit) {
  values <- unique(values[is.finite(values)])
  if (length(values) == 0L) return(values)
  ordered <- order(abs(values - anchor), values)
  keep <- ordered[seq_len(min(as.integer(limit), length(values)))]
  values[sort(keep)]
}

cutoff_route_candidates <- function(source, box, panel_width, panel_height,
                                    obstacles = list()) {
  defaults <- .cutoff_layout_defaults
  attachment <- cutoff_nearest_box_port(source, box)
  port <- attachment$point
  panel_margin <- defaults$route_panel_margin
  requested_gaps <- defaults$route_gaps
  tolerance <- defaults$route_tolerance
  if (attachment$angle %in% c(90, 270)) {
    outward <- if (attachment$angle == 270) -1 else 1
    room <- if (outward < 0) {
      port[[2L]] - panel_margin
    } else {
      panel_height - panel_margin - port[[2L]]
    }
    gaps <- unique(pmin(requested_gaps, max(defaults$route_min_room, room)))
    obstacle_tracks <- unlist(lapply(obstacles, function(obstacle) {
      if (outward < 0) {
        obstacle$y1 - requested_gaps
      } else {
        obstacle$y2 + requested_gaps
      }
    }), use.names = FALSE)
    tracks <- unique(c(port[[2L]] + outward * gaps, obstacle_tracks))
    tracks <- pmin(panel_height - panel_margin, pmax(panel_margin, tracks))
    tracks <- tracks[outward * (tracks - port[[2L]]) > tolerance]
    tracks <- cutoff_nearby_route_positions(
      tracks, port[[2L]], defaults$route_track_limit
    )
    axis_first <- lapply(tracks, function(track) {
      rbind(source, c(source[[1L]], track), c(port[[1L]], track), port)
    })
    inward <- sign(port[[1L]] - source[[1L]])
    if (!is.finite(inward) || inward == 0) inward <- 1
    box_edge <- if (inward > 0) box$x1 else box$x2
    edge_detours <- box_edge - inward * requested_gaps
    obstacle_detours <- unlist(lapply(obstacles, function(obstacle) {
      c(obstacle$x1 - requested_gaps, obstacle$x2 + requested_gaps)
    }), use.names = FALSE)
    detours <- unique(c(
      source[[1L]] + inward * defaults$route_detour_offsets,
      edge_detours, obstacle_detours
    ))
    detours <- detours[abs(detours - source[[1L]]) > tolerance]
    detours <- pmin(panel_width - panel_margin, pmax(panel_margin, detours))
    detours <- cutoff_nearby_route_positions(
      detours, source[[1L]], defaults$route_detour_limit
    )
    depth_first <- unlist(lapply(detours, function(detour) {
      lapply(tracks, function(track) {
        rbind(
          source, c(detour, source[[2L]]), c(detour, track),
          c(port[[1L]], track), port
        )
      })
    }), recursive = FALSE)
    c(axis_first, depth_first)
  } else {
    outward <- if (attachment$angle == 180) -1 else 1
    room <- if (outward < 0) {
      port[[1L]] - panel_margin
    } else {
      panel_width - panel_margin - port[[1L]]
    }
    gaps <- unique(pmin(requested_gaps, max(defaults$route_min_room, room)))
    obstacle_tracks <- unlist(lapply(obstacles, function(obstacle) {
      if (outward < 0) {
        obstacle$x1 - requested_gaps
      } else {
        obstacle$x2 + requested_gaps
      }
    }), use.names = FALSE)
    tracks <- unique(c(port[[1L]] + outward * gaps, obstacle_tracks))
    tracks <- pmin(panel_width - panel_margin, pmax(panel_margin, tracks))
    tracks <- tracks[outward * (tracks - port[[1L]]) > tolerance]
    tracks <- cutoff_nearby_route_positions(
      tracks, port[[1L]], defaults$route_track_limit
    )
    axis_first <- lapply(tracks, function(track) {
      rbind(source, c(track, source[[2L]]), c(track, port[[2L]]), port)
    })
    inward <- sign(port[[2L]] - source[[2L]])
    if (!is.finite(inward) || inward == 0) inward <- 1
    box_edge <- if (inward > 0) box$y1 else box$y2
    edge_detours <- box_edge - inward * requested_gaps
    obstacle_detours <- unlist(lapply(obstacles, function(obstacle) {
      c(obstacle$y1 - requested_gaps, obstacle$y2 + requested_gaps)
    }), use.names = FALSE)
    detours <- unique(c(
      source[[2L]] + inward * defaults$route_detour_offsets,
      edge_detours, obstacle_detours
    ))
    detours <- detours[abs(detours - source[[2L]]) > tolerance]
    detours <- pmin(panel_height - panel_margin, pmax(panel_margin, detours))
    detours <- cutoff_nearby_route_positions(
      detours, source[[2L]], defaults$route_detour_limit
    )
    depth_first <- unlist(lapply(detours, function(detour) {
      lapply(tracks, function(track) {
        rbind(
          source, c(source[[1L]], detour), c(track, detour),
          c(track, port[[2L]]), port
        )
      })
    }), recursive = FALSE)
    c(axis_first, depth_first)
  }
}

cutoff_segment_hits_box <- function(from, to, box) {
  tolerance <- .cutoff_layout_defaults$segment_tolerance
  if (sum(abs(to - from)) < tolerance) return(FALSE)
  if (abs(from[[2L]] - to[[2L]]) < tolerance) {
    return(
      from[[2L]] > box$y1 + tolerance &&
        from[[2L]] < box$y2 - tolerance &&
        max(min(from[[1L]], to[[1L]]), box$x1) <
          min(max(from[[1L]], to[[1L]]), box$x2) - tolerance
    )
  }
  if (abs(from[[1L]] - to[[1L]]) < tolerance) {
    return(
      from[[1L]] > box$x1 + tolerance &&
        from[[1L]] < box$x2 - tolerance &&
        max(min(from[[2L]], to[[2L]]), box$y1) <
          min(max(from[[2L]], to[[2L]]), box$y2) - tolerance
    )
  }
  FALSE
}

cutoff_route_box_hits <- function(route, box) {
  sum(vapply(seq_len(nrow(route) - 1L), function(i) {
    cutoff_segment_hits_box(route[i, ], route[i + 1L, ], box)
  }, logical(1)))
}

cutoff_route_conflict <- function(first, second) {
  defaults <- .cutoff_layout_defaults
  cost <- 0
  tolerance <- defaults$route_tolerance
  for (i in seq_len(nrow(first) - 1L)) {
    a <- first[i, ]
    b <- first[i + 1L, ]
    first_horizontal <- abs(a[[2L]] - b[[2L]]) < tolerance
    for (j in seq_len(nrow(second) - 1L)) {
      p <- second[j, ]
      q <- second[j + 1L, ]
      second_horizontal <- abs(p[[2L]] - q[[2L]]) < tolerance
      if (identical(first_horizontal, second_horizontal)) {
        same_track <- if (first_horizontal) {
          abs(a[[2L]] - p[[2L]]) < tolerance
        } else {
          abs(a[[1L]] - p[[1L]]) < tolerance
        }
        if (!same_track) next
        first_range <- if (first_horizontal) {
          sort(c(a[[1L]], b[[1L]]))
        } else {
          sort(c(a[[2L]], b[[2L]]))
        }
        second_range <- if (first_horizontal) {
          sort(c(p[[1L]], q[[1L]]))
        } else {
          sort(c(p[[2L]], q[[2L]]))
        }
        if (max(first_range[[1L]], second_range[[1L]]) <
              min(first_range[[2L]], second_range[[2L]]) - tolerance) {
          cost <- cost + defaults$shared_track_cost
        }
      } else {
        horizontal_segment <- if (first_horizontal) rbind(a, b) else rbind(p, q)
        vertical_segment <- if (first_horizontal) rbind(p, q) else rbind(a, b)
        crossing <-
          vertical_segment[1L, 1L] >= min(horizontal_segment[, 1L]) - tolerance &&
          vertical_segment[1L, 1L] <= max(horizontal_segment[, 1L]) + tolerance &&
          horizontal_segment[1L, 2L] >= min(vertical_segment[, 2L]) - tolerance &&
          horizontal_segment[1L, 2L] <= max(vertical_segment[, 2L]) + tolerance
        if (crossing) cost <- cost + defaults$crossing_cost
      }
    }
  }
  cost
}

choose_cutoff_route <- function(routes, boxes, existing, new_box) {
  defaults <- .cutoff_layout_defaults
  scores <- vapply(routes, function(route) {
    box_hits <- sum(vapply(
      c(boxes, list(new_box)),
      function(box) cutoff_route_box_hits(route, box), numeric(1)
    ))
    old_route_hits_box <- sum(vapply(
      existing, function(old) cutoff_route_box_hits(old, new_box), numeric(1)
    ))
    conflicts <- sum(vapply(
      existing, cutoff_route_conflict, numeric(1), second = route
    ))
    route_length <- sum(sqrt(rowSums(
      (route[-1L, , drop = FALSE] -
         route[-nrow(route), , drop = FALSE])^2
    )))
    defaults$box_hit_cost * (box_hits + old_route_hits_box) + conflicts +
      defaults$route_length_cost * route_length
  }, numeric(1))
  routes[[which.min(scores)]]
}

cutoff_stem_source <- function(row, horizontal, height, panel_width,
                               panel_height) {
  if (horizontal) {
    inward <- sign(row$.opposite_x - row$x)
    if (!is.finite(inward) || inward == 0) inward <- 1
    return(c(
      row$x * panel_width + inward * min(
        height * panel_width,
        abs(row$.opposite_x - row$x) * panel_width
      ),
      row$y * panel_height
    ))
  }
  inward <- sign(row$.opposite_y - row$y)
  if (!is.finite(inward) || inward == 0) inward <- 1
  c(
    row$x * panel_width,
    row$y * panel_height + inward * min(
      height * panel_height,
      abs(row$.opposite_y - row$y) * panel_height
    )
  )
}

#' Jointly place all cutoff labels in the panel
#'
#' @param data Transformed labelled cutoff rows.
#' @param horizontal Whether the cutoff stems run horizontally.
#' @param height Stem height as a panel fraction.
#' @param avoidance Transformed distribution occupancy profile.
#' @param metrics Precomputed label dimensions.
#' @param panel_width,panel_height Panel dimensions in millimetres.
#'
#' @return Boxes, routes, and physical sides in input order.
#' @noRd
solve_cutoff_callout_layout <- function(data, metrics, panel_width,
                                        panel_height, horizontal = FALSE,
                                        height = 0.2, avoidance = NULL) {
  avoidance <- prepare_cutoff_avoidance(
    avoidance, horizontal, panel_width, panel_height
  )
  physical <- cutoff_physical_sides(data)
  candidates <- lapply(seq_len(nrow(data)), function(i) {
    cutoff_box_candidates(
      data[i, , drop = FALSE], metrics$width[[i]], metrics$height[[i]],
      physical[[i]], horizontal, height, panel_width, panel_height,
      nrow(data), avoidance
    )
  })
  anchors <- data$.screen
  call_id <- if ("call_id" %in% names(data)) data$call_id else seq_len(nrow(data))
  placement_order <- order(
    match(physical, c("lower", "upper")),
    ifelse(physical == "lower", -anchors, anchors), call_id
  )

  states <- list(list(
    cost = 0, boxes = vector("list", nrow(data)), placed = integer()
  ))
  for (index in placement_order) {
    expanded <- list()
    at <- 0L
    for (state in states) {
      placed_boxes <- state$boxes[state$placed]
      for (candidate in candidates[[index]]) {
        collides <- any(vapply(
          placed_boxes, cutoff_boxes_overlap, logical(1), second = candidate
        ))
        if (collides) next

        next_state <- state
        next_state$cost <- state$cost + candidate$cost
        next_state$boxes[[index]] <- candidate
        next_state$placed <- c(state$placed, index)
        at <- at + 1L
        expanded[[at]] <- next_state
      }
    }
    if (length(expanded) == 0L) {
      for (state in states) {
        placed_boxes <- state$boxes[state$placed]
        for (candidate in candidates[[index]]) {
          overlap_count <- sum(vapply(
            placed_boxes, cutoff_boxes_overlap, logical(1), second = candidate
          ))
          next_state <- state
          next_state$cost <- state$cost + candidate$cost +
            .cutoff_layout_defaults$overlap_cost * overlap_count
          next_state$boxes[[index]] <- candidate
          next_state$placed <- c(state$placed, index)
          at <- at + 1L
          expanded[[at]] <- next_state
        }
      }
    }
    costs <- vapply(expanded, `[[`, numeric(1), "cost")
    states <- expanded[order(costs)[seq_len(min(
      .cutoff_layout_defaults$beam_width, length(costs)
    ))]]
  }

  best <- states[[1L]]
  route_order <- order(
    match(physical, c("lower", "upper")),
    ifelse(physical == "lower", anchors, -anchors), call_id
  )
  routes <- vector("list", nrow(data))
  attachment_angles <- numeric(nrow(data))
  routed <- integer()
  for (index in route_order) {
    source <- cutoff_stem_source(
      data[index, , drop = FALSE], horizontal, height,
      panel_width, panel_height
    )
    other_boxes <- best$boxes[setdiff(seq_len(nrow(data)), index)]
    attachment <- cutoff_nearest_box_port(source, best$boxes[[index]])
    routes[[index]] <- choose_cutoff_route(
      cutoff_route_candidates(
        source, best$boxes[[index]], panel_width, panel_height, other_boxes
      ),
      other_boxes, routes[routed], best$boxes[[index]]
    )
    attachment_angles[[index]] <- attachment$angle
    routed <- c(routed, index)
  }
  box_fields <- c("x1", "x2", "y1", "y2", "x", "y")
  boxes <- lapply(best$boxes, function(box) box[box_fields])
  list(
    boxes = boxes, routes = routes, physical = physical,
    horizontal = horizontal, attachment_angles = attachment_angles
  )
}
