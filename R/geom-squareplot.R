#' Choose a binwidth the way a countable histogram wants one
#'
#' @param x A numeric vector.
#'
#' @return A single number.
#'
#' @noRd
squareplot_binwidth <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) {
    return(1)
  }
  is_integer_like <- all(abs(x - round(x)) < 1e-7, na.rm = TRUE)
  if (is_integer_like && diff(rng) <= 50) 1 else diff(rng) / 30
}

#' Place the bin grid so the smallest value lands at the start of a bin
#'
#' @param x A numeric vector.
#' @param binwidth The width of a bin.
#'
#' @return A single number.
#'
#' @noRd
squareplot_origin <- function(x, binwidth) {
  floor(min(x, na.rm = TRUE) / binwidth) * binwidth
}

#' Bin observations for a countable-rectangle histogram
#'
#' Counts observations per bin on a fixed grid. [GeomSquareplot] re-expands each
#' bin into one rectangle per observation. Pair the two in a `ggplot2::layer()`
#' to put countable squares in a plot you are assembling yourself.
#'
#' @format A [ggplot2::Stat] object.
#'
#' @seealso [gf_squareplot()], which pairs this stat and geom for you.
#' @export
StatSquareplot <- ggplot2::ggproto(
  "StatSquareplot", ggplot2::StatBindot,
  setup_params = function(data, params) {
    params$binaxis <- "x"
    values <- data$x[!is.na(data$x)]
    if (length(values) == 0) {
      params$binwidth <- params$binwidth %||% 1
      params$origin <- params$origin %||% 0
      return(params)
    }
    if (is.null(params$binwidth)) params$binwidth <- squareplot_binwidth(values)
    if (is.null(params$origin)) params$origin <- squareplot_origin(values, params$binwidth)
    params
  },
  # StatBindot's histodot path stops its break vector at max(x), folding the
  # largest value into the bin below it; the floor() rule gives it its own bin
  compute_group = function(self, data, scales, binwidth = NULL, binaxis = "x",
                           origin = NULL, drop = FALSE, ...) {
    values <- data$x[!is.na(data$x)]
    if (length(values) == 0) {
      return(data.frame(
        count = numeric(0), x = numeric(0), xmin = numeric(0),
        xmax = numeric(0), binwidth = numeric(0), width = numeric(0)
      ))
    }

    bin <- floor((values - origin) / binwidth)
    bins <- seq(min(bin), max(bin))
    count <- as.numeric(tabulate(bin - min(bin) + 1L, nbins = length(bins)))
    if (drop) {
      keep <- count > 0
      bins <- bins[keep]
      count <- count[keep]
    }

    xmin <- origin + bins * binwidth
    data.frame(
      count = count, x = xmin + binwidth / 2,
      xmin = xmin, xmax = xmin + binwidth,
      binwidth = binwidth, width = binwidth
    )
  }
)

#' Draw one countable rectangle per observation
#'
#' Re-expands each bin from [StatSquareplot] into one unit rectangle per
#' observation, stacked. The rectangles are one unit tall in data units, so the
#' y axis is a real count. Pair it with [StatSquareplot] in a
#' `ggplot2::layer()` to draw the squares yourself, where a mapped `fill` stacks
#' its groups within each bin.
#'
#' The `bar` parameter chooses what a bin is drawn as, on one grid and in one
#' layer: `"none"` draws the squares, `"outline"` frames them with the bar they
#' add up to, and `"solid"` draws that bar with the squares covered over. The
#' bar is derived from the squares rather than re-binned, so a bin holding no
#' observations has no bar, and a solid bar stacks a mapped `fill` in the same
#' spans its squares did. `bar_colour` and `bar_linewidth` belong to the bar;
#' `colour` and `linewidth` are the separators between the squares.
#'
#' @format A [ggplot2::Geom] object.
#'
#' @seealso [gf_squareplot()], which pairs this stat and geom for you.
#' @export
GeomSquareplot <- ggplot2::ggproto(
  "GeomSquareplot", ggplot2::GeomRect,
  # the rectangles do not exist until setup_data() builds them out of the counts,
  # and required aesthetics are checked before that runs, so GeomRect's four
  # corners are the wrong requirement to inherit
  required_aes = "x",
  extra_params = c("na.rm"),
  # record whether linewidth was supplied before the theme default fills it in
  use_defaults = function(self, data, params = list(), ...) {
    supplied <- !is.null(data$linewidth) || !is.null(params$linewidth)
    data <- ggplot2::ggproto_parent(ggplot2::GeomRect, self)$use_defaults(data, params, ...)
    if (nrow(data) > 0) data$fit_border <- !supplied
    data
  },
  setup_data = function(data, params) {
    data <- data[rep(seq_len(nrow(data)), data$count), , drop = FALSE]
    if (nrow(data) == 0) {
      return(data)
    }

    # stacking groups in group order is what lets a mapped fill stack correctly
    key <- interaction(data$PANEL, data$xmin, drop = TRUE, lex.order = TRUE)
    ord <- order(key, data$group)
    data <- data[ord, , drop = FALSE]
    idx <- stats::ave(seq_len(nrow(data)), key[ord], FUN = function(z) seq_along(z) - 1L)

    data$ymin <- idx
    data$ymax <- data$ymin + 1
    data$y <- (data$ymin + data$ymax) / 2
    rownames(data) <- NULL
    data
  },
  setup_params = function(self, data, params) {
    params$bar <- params$bar %||% "none"
    if (params$bar %in% c("none", "outline", "solid") == FALSE) {
      abort(c(
        "`bar` must be one of \"none\", \"outline\" or \"solid\"",
        glue("found: {deparse(params$bar)}")
      ))
    }
    ggplot2::ggproto_parent(ggplot2::GeomRect, self)$setup_params(data, params)
  },
  draw_panel = function(self, data, panel_params, coord, bar = "none",
                        bar_colour = NULL, bar_linewidth = 0.5,
                        lineend = "butt", linejoin = "mitre") {
    if (nrow(data) == 0) {
      return(ggplot2::zeroGrob())
    }

    bar_grob <- NULL
    if (!identical(bar, "none")) {
      # the solid bar replaces the squares, so it has to show what they showed:
      # one span per group in the bin, stacked, rather than one rectangle per bin
      rows <- squareplot_bar_rows(data, by_group = identical(bar, "solid"))
      rows$colour <- bar_colour %||% "grey20"
      rows$linewidth <- bar_linewidth
      rows$linetype <- 1
      if (identical(bar, "outline")) {
        rows$fill <- NA
        rows$alpha <- NA
      }
      bar_grob <- ggplot2::GeomRect$draw_panel(
        rows, panel_params, coord, lineend = lineend, linejoin = linejoin
      )
    }
    if (identical(bar, "solid")) {
      return(bar_grob)
    }

    fit <- isTRUE(data$fit_border[1])
    rects <- ggplot2::ggproto_parent(ggplot2::GeomRect, self)$draw_panel(
      data, panel_params, coord, lineend = lineend, linejoin = linejoin
    )
    # a square's rendered size is not known until the device is; defer to draw time
    squares <- grid::gTree(
      children = grid::gList(rects),
      width_frac = (data$xmax[[1]] - data$xmin[[1]]) / diff(panel_params$x.range),
      height_frac = (data$ymax[[1]] - data$ymin[[1]]) / diff(panel_params$y.range),
      linewidth = data$linewidth[[1]], fit = fit, n = nrow(data),
      cl = "coursekata_squares"
    )
    if (is.null(bar_grob)) squares else grid::grobTree(squares, bar_grob)
  },
  draw_key = ggplot2::draw_key_rect
)

#' Collapse a panel's squares back into the rectangles a bar is drawn from
#'
#' @param data A panel's worth of expanded square rows.
#' @param by_group Whether to keep one rectangle per group within a bin rather
#'   than one per bin.
#'
#' @return One row per bin, or per group within a bin, spanning the squares it
#'   was collapsed from and keeping their aesthetics.
#'
#' @noRd
squareplot_bar_rows <- function(data, by_group = FALSE) {
  key <- interaction(data$PANEL, data$xmin, drop = TRUE, lex.order = TRUE)
  if (by_group) key <- interaction(key, data$group, drop = TRUE, lex.order = TRUE)
  first <- !duplicated(key)
  id <- as.character(key[first])
  bars <- data[first, , drop = FALSE]
  bars$ymin <- as.numeric(tapply(data$ymin, key, min)[id])
  bars$ymax <- as.numeric(tapply(data$ymax, key, max)[id])
  rownames(bars) <- NULL
  bars
}

#' Fit a square's border to the square
#'
#' A border wider than the square it outlines erases the square, which at large
#' sample sizes turns a countable plot into a blank panel. Cap it at a quarter of
#' the square's smaller side so the marks stay separable as they shrink.
#'
#' @param width_pt,height_pt The square's rendered size, in points.
#' @param linewidth The requested linewidth, in the usual ggplot2 units.
#' @param fit Whether to cap the stroke. `FALSE` honors `linewidth` exactly.
#'
#' @return The stroke width to draw, in points.
#'
#' @noRd
fit_square_border <- function(width_pt, height_pt, linewidth, fit = TRUE) {
  requested <- linewidth * ggplot2::.pt
  if (!fit) {
    return(requested)
  }
  min(requested, min(width_pt, height_pt) / 4)
}

#' Size a squareplot's borders once the device is known
#'
#' How wide a square is drawn depends on the device, which is not known while
#' the plot is being built, so the border cannot be fitted until the panel is
#' actually being drawn. Inside this hook the current viewport is the panel, so
#' one npc is the panel's width and the square's own size follows from the
#' fraction of the panel it occupies.
#'
#' @param x A `coursekata_squares` gTree.
#'
#' @return The gTree, with its borders fitted to the drawn squares.
#'
#' @noRd
#' @exportS3Method grid::makeContent
makeContent.coursekata_squares <- function(x) {
  width_pt <- x$width_frac * grid::convertWidth(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  height_pt <- x$height_frac * grid::convertHeight(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  stroke <- fit_square_border(width_pt, height_pt, x$linewidth, fit = x$fit)
  smaller <- min(width_pt, height_pt)

  if (!x$fit && stroke >= smaller) {
    warn(c(
      glue(
        "{x$n} observations are hidden behind their own borders: ",
        "each square is {signif(smaller, 2)} pt across ",
        "but its border is {signif(stroke, 2)} pt wide"
      ),
      i = "leave `linewidth` unset to fit the border to the square"
    ), class = "coursekata_squares_hidden")
  }

  rects <- x$children[[1]]
  rects$gp$lwd <- stroke
  grid::setChildren(x, grid::gList(rects))
}
