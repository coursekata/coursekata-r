# Visual snapshots of gf_b(): whether the layout reads correctly -- do the
# labels overlap the arrows, is the run label on the correct side of the
# rise. Everything numeric is asserted in test-gf_b.R; these two exist only
# for what an assertion cannot state.

b_visual_strict_overlap <- function(a0, a1, b0, b1, tolerance = 0.05) {
  min(a1, b1) - max(a0, b0) > tolerance
}

b_visual_segment_hits_box <- function(segment, box, tolerance = 0.05) {
  bounds <- c(
    left = box$left + tolerance, right = box$right - tolerance,
    bottom = box$bottom + tolerance, top = box$top - tolerance
  )
  if (bounds[["left"]] >= bounds[["right"]] ||
      bounds[["bottom"]] >= bounds[["top"]]) {
    return(FALSE)
  }
  delta <- c(x = segment$x1 - segment$x0, y = segment$y1 - segment$y0)
  p <- c(-delta[["x"]], delta[["x"]], -delta[["y"]], delta[["y"]])
  q <- c(
    segment$x0 - bounds[["left"]], bounds[["right"]] - segment$x0,
    segment$y0 - bounds[["bottom"]], bounds[["top"]] - segment$y0
  )
  lower <- 0
  upper <- 1
  for (i in seq_along(p)) {
    if (abs(p[[i]]) < 1e-12) {
      if (q[[i]] < 0) return(FALSE)
    } else if (p[[i]] < 0) {
      lower <- max(lower, q[[i]] / p[[i]])
    } else {
      upper <- min(upper, q[[i]] / p[[i]])
    }
  }
  lower <= upper
}

b_visual_collisions <- function(plot, width, height) {
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = width, height = height, units = "in", res = 144)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)

  table <- ggplot2::ggplotGrob(plot)
  grid::grid.newpage()
  grid::grid.draw(table)
  grid::grid.force()
  panel_indices <- grep("^panel", table$layout$name)
  tags <- vapply(plot$layers, function(layer) {
    tag <- attr(layer, "coursekata_layer")
    if (is.null(tag)) "base" else tag
  }, character(1))
  package_layers <- which(tags != "base")
  findings <- character()

  for (panel_number in seq_along(panel_indices)) {
    panel_index <- panel_indices[[panel_number]]
    panel <- table$grobs[[panel_index]]
    wrapper <- paste0(
      table$layout$name[[panel_index]], ".",
      table$layout$t[[panel_index]], "-", table$layout$l[[panel_index]], "-",
      table$layout$b[[panel_index]], "-", table$layout$r[[panel_index]]
    )
    grid::seekViewport(wrapper)
    grid::pushViewport(panel$vp)
    panel_width <- grid::convertWidth(
      grid::unit(1, "npc"), "mm", valueOnly = TRUE
    )
    panel_height <- grid::convertHeight(
      grid::unit(1, "npc"), "mm", valueOnly = TRUE
    )

    items <- list()
    for (layer_index in package_layers) {
      grob <- panel$children[[2L + layer_index]]
      tag <- tags[[layer_index]]
      if (inherits(grob, "text")) {
        grob_width <- grid::convertWidth(
          grid::grobWidth(grob), "mm", valueOnly = TRUE
        )
        grob_height <- grid::convertHeight(
          grid::grobHeight(grob), "mm", valueOnly = TRUE
        )
        x <- as.numeric(grob$x) * panel_width
        y <- as.numeric(grob$y) * panel_height
        items[[length(items) + 1L]] <- list(
          tag = tag, type = "text",
          left = x - grob$hjust * grob_width,
          right = x + (1 - grob$hjust) * grob_width,
          bottom = y - grob$vjust * grob_height,
          top = y + (1 - grob$vjust) * grob_height
        )
        if (!is.finite(grob_width) || !is.finite(grob_height) ||
            grob_width <= 0 || grob_height <= 0) {
          findings <- c(
            findings,
            paste("panel", panel_number, tag, "has no measurable text grob")
          )
        }
      } else if (inherits(grob, "segments")) {
        segment <- list(
          tag = tag, type = "segment",
          x0 = as.numeric(grob$x0) * panel_width,
          x1 = as.numeric(grob$x1) * panel_width,
          y0 = as.numeric(grob$y0) * panel_height,
          y1 = as.numeric(grob$y1) * panel_height
        )
        items[[length(items) + 1L]] <- segment
        arrow <- plot$layers[[layer_index]]$geom_params$arrow
        length <- sqrt(
          (segment$x1 - segment$x0)^2 + (segment$y1 - segment$y0)^2
        )
        if (!is.null(arrow) && length > 1e-8) {
          direction <- c(
            x = (segment$x1 - segment$x0) / length,
            y = (segment$y1 - segment$y0) / length
          )
          arrow_length <- grid::convertWidth(
            arrow$length, "mm", valueOnly = TRUE
          )
          angle <- arrow$angle * pi / 180
          back <- c(x = segment$x1, y = segment$y1) -
            direction * arrow_length * cos(angle)
          side <- c(x = -direction[["y"]], y = direction[["x"]]) *
            arrow_length * sin(angle)
          for (tip in list(back + side, back - side)) {
            items[[length(items) + 1L]] <- list(
              tag = paste0(tag, "_arrowhead"), type = "segment",
              x0 = segment$x1, x1 = tip[["x"]],
              y0 = segment$y1, y1 = tip[["y"]]
            )
          }
        }
      } else if (inherits(grob, "points")) {
        diameter <- grid::convertWidth(
          grid::unit(grob$gp$fontsize[[1]], "pt"), "mm", valueOnly = TRUE
        )
        x <- as.numeric(grob$x) * panel_width
        y <- as.numeric(grob$y) * panel_height
        items[[length(items) + 1L]] <- list(
          tag = tag, type = "point",
          left = x - diameter / 2, right = x + diameter / 2,
          bottom = y - diameter / 2, top = y + diameter / 2
        )
      } else {
        findings <- c(
          findings,
          paste("panel", panel_number, tag, "did not render its expected grob")
        )
      }
    }

    texts <- Filter(function(item) item$type == "text", items)
    others <- Filter(function(item) item$type != "text", items)
    for (i in seq_along(texts)) {
      text <- texts[[i]]
      clipped <- text$left < -0.05 || text$right > panel_width + 0.05 ||
        text$bottom < -0.05 || text$top > panel_height + 0.05
      if (clipped) {
        findings <- c(
          findings,
          paste("panel", panel_number, text$tag, "is clipped")
        )
      }

      if (i < length(texts)) {
        for (j in seq.int(i + 1L, length(texts))) {
          other <- texts[[j]]
          if (b_visual_strict_overlap(
            text$left, text$right, other$left, other$right
          ) && b_visual_strict_overlap(
            text$bottom, text$top, other$bottom, other$top
          )) {
            findings <- c(
              findings,
              paste("panel", panel_number, text$tag, "overlaps", other$tag)
            )
          }
        }
      }

      for (other in others) {
        hit <- if (other$type == "segment") {
          b_visual_segment_hits_box(other, text)
        } else {
          b_visual_strict_overlap(
            text$left, text$right, other$left, other$right
          ) && b_visual_strict_overlap(
            text$bottom, text$top, other$bottom, other$top
          )
        }
        if (hit) {
          findings <- c(
            findings,
            paste("panel", panel_number, text$tag, "overlaps", other$tag)
          )
        }
      }
    }

    item <- function(tag, type) {
      found <- Filter(function(value) value$tag == tag && value$type == type, items)
      if (length(found) == 1L) found[[1L]] else NULL
    }
    centre <- function(box) {
      c(x = (box$left + box$right) / 2, y = (box$bottom + box$top) / 2)
    }
    rise <- item("b1", "segment")
    run <- item("run", "segment")
    b1_label <- item("b1_label", "text")
    run_label <- item("run_label", "text")
    if (!is.null(rise) && !is.null(run) && !is.null(b1_label) && !is.null(run_label)) {
      joint <- c(x = rise$x1, y = rise$y1)
      rise_vector <- joint - c(x = rise$x0, y = rise$y0)
      run_vector <- c(x = run$x1, y = run$y1) - c(x = run$x0, y = run$y0)
      if (sqrt(sum(run_vector^2)) > 0.05 &&
          sum((centre(b1_label) - joint) * run_vector) >= 0) {
        findings <- c(
          findings,
          paste("panel", panel_number, "b1_label is not opposite the run")
        )
      }
      if (sqrt(sum(rise_vector^2)) > 0.05 &&
          sum((centre(run_label) - joint) * rise_vector) >= 0) {
        findings <- c(
          findings,
          paste("panel", panel_number, "run_label is outside the triangle")
        )
      }
    }
    grid::upViewport(2)
  }

  unique(findings)
}

b_visual_transform_cases <- function(plot, model) {
  annotated <- gf_b(plot, model)
  list(
    ordinary = annotated,
    x_reverse = annotated + ggplot2::scale_x_reverse(),
    y_reverse = annotated + ggplot2::scale_y_reverse(),
    both_reverse = annotated +
      ggplot2::scale_x_reverse() + ggplot2::scale_y_reverse(),
    flip_after = annotated + ggplot2::coord_flip(),
    flip_before = gf_b(plot + ggplot2::coord_flip(), model),
    flip_x_reverse = annotated +
      ggplot2::scale_x_reverse() + ggplot2::coord_flip(),
    flip_y_reverse = annotated +
      ggplot2::scale_y_reverse() + ggplot2::coord_flip(),
    flip_both_reverse = annotated +
      ggplot2::scale_x_reverse() + ggplot2::scale_y_reverse() +
      ggplot2::coord_flip()
  )
}

b_visual_named_transform_cases <- function(prefix, plot, model) {
  cases <- b_visual_transform_cases(plot, model)
  stats::setNames(cases, paste0(prefix, "_", names(cases)))
}

test_that("the continuous rise-over-run triangle renders", {
  model <- lm(Thumb ~ Height, data = Fingers)
  gf_point(Thumb ~ Height, data = Fingers, alpha = .3) %>%
    gf_b(model) %>%
    expect_doppelganger("gf_b continuous triangle")
})

test_that("the 3-group categorical arrows render", {
  set.seed(41)
  df <- data.frame(y = rnorm(60, 10, 3), g = factor(rep(c("a", "b", "c"), each = 20)))
  model <- lm(y ~ g, data = df)
  gf_jitter(y ~ g, data = df, width = .1, seed = 41) %>%
    gf_b(model) %>%
    expect_doppelganger("gf_b categorical arrows")
})

test_that("coefficient labels clear package marks at supported device sizes", {
  positive_fit <- lm(Thumb ~ Height, data = Fingers)

  negative <- data.frame(x = seq(-20, 20, length.out = 81))
  negative$y <- 50 - 2.25 * negative$x + sin(negative$x)
  negative_fit <- lm(y ~ x, data = negative)

  flat <- data.frame(x = seq(-20, 20, length.out = 81), y = 50)
  flat_fit <- lm(y ~ x, data = flat)

  set.seed(41)
  categorical <- data.frame(
    g = factor(rep(c("a", "b", "c"), each = 24)),
    y = c(rnorm(24, 10, 1.3), rnorm(24, 15, 1.3), rnorm(24, 6, 1.3))
  )
  categorical_fit <- lm(y ~ g, data = categorical)

  faceted <- transform(
    Fingers,
    panel = factor(
      rep(c("one", "two", "three", "four", "five"), length.out = nrow(Fingers))
    )
  )
  facet_fit <- lm(Thumb ~ Height, data = faceted)

  plots <- list(
    continuous_positive = gf_point(
      Thumb ~ Height, data = Fingers, alpha = 0.3
    ) |> gf_b(positive_fit),
    continuous_negative = gf_point(
      y ~ x, data = negative, alpha = 0.3
    ) |> gf_b(negative_fit),
    continuous_transposed = gf_point(
      Height ~ Thumb, data = Fingers, alpha = 0.3
    ) |> gf_b(positive_fit),
    continuous_negative_run = gf_point(
      Thumb ~ Height, data = Fingers, alpha = 0.3
    ) |> gf_b(positive_fit, run = -5, run_x = 65),
    continuous_zero_slope = gf_point(
      y ~ x, data = flat, alpha = 0.3
    ) |> gf_b(flat_fit, run = 10, run_x = -10),
    categorical = gf_jitter(
      y ~ g, data = categorical, width = 0.1, seed = 41
    ) |> gf_b(categorical_fit),
    categorical_transposed = gf_jitter(
      g ~ y, data = categorical, height = 0.1, seed = 41
    ) |> gf_b(categorical_fit),
    facet_continuous = gf_point(
      Thumb ~ Height, data = faceted, alpha = 0.25
    ) |>
      gf_facet_wrap(~panel, nrow = 1) |>
      gf_b(facet_fit)
  )

  transformed <- c(
    b_visual_named_transform_cases(
      "positive",
      gf_point(Thumb ~ Height, data = Fingers, alpha = 0.3), positive_fit
    ),
    b_visual_named_transform_cases(
      "negative", gf_point(y ~ x, data = negative, alpha = 0.3), negative_fit
    ),
    b_visual_named_transform_cases(
      "transposed",
      gf_point(Height ~ Thumb, data = Fingers, alpha = 0.3), positive_fit
    )
  )
  plots <- c(
    plots, transformed,
    list(
      continuous_log_predictor = gf_point(
        Thumb ~ Height, data = Fingers, alpha = 0.3
      ) |>
        gf_b(positive_fit, show_b0 = FALSE) +
        ggplot2::scale_x_log10()
    )
  )

  for (name in names(plots)) {
    for (size in list(c(4, 3), c(6, 4), c(8, 6))) {
      collisions <- b_visual_collisions(plots[[name]], size[[1]], size[[2]])
      expect_equal(
        collisions, character(),
        info = paste(name, paste(size, collapse = "x"), collisions)
      )
    }
  }
})
