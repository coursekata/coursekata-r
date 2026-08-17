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

#' Refuse a y scale a squareplot's stat cannot draw into
#'
#' `Layer$compute_statistic` is the only build-time hook with `layout` in scope,
#' so this is where the guard has to run regardless of which parent is binning:
#' [ggplot2::StatBin] for a continuous x, [ggplot2::StatCount] for a discrete one.
#'
#' @param data The layer's data, one row per panel at minimum.
#' @param layout The plot's `Layout`.
#'
#' @return Invisible `NULL`, or an abort.
#'
#' @noRd
squareplot_check_panels <- function(data, layout) {
  for (panel in unique(data$PANEL)) {
    squareplot_check_y_scale(layout$get_scales(panel)$y)
  }
  invisible(NULL)
}

#' Bin observations the way `ggplot2::stat_bin()` does
#'
#' `StatSquareplot` *is* `ggplot2::StatBin`: `binwidth`, `bins`, `center`,
#' `boundary`, `closed`, `breaks` and `pad` all mean exactly what they mean on
#' a histogram, because they are the histogram's own code. [GeomSquareplot]
#' re-expands each bin into one rectangle per observation. Pair the two in a
#' `ggplot2::layer()` to put countable squares in a plot you are assembling
#' yourself.
#'
#' The one difference from a plain histogram is the binwidth chosen when the
#' call names no grid at all: integer-valued data over a small range gets one
#' bin per integer, rather than `stat_bin`'s `bins = 30` default, because
#' thirty slivers is the wrong advice for a plot whose point is countability.
#'
#' @format A [ggplot2::Stat] object.
#'
#' @seealso [gf_squareplot()], which pairs this stat and geom for you.
#' @export
StatSquareplot <- ggplot2::ggproto(
  "StatSquareplot", ggplot2::StatBin,
  compute_layer = function(self, data, params, layout) {
    squareplot_check_panels(data, layout)
    ggplot2::ggproto_parent(ggplot2::StatBin, self)$compute_layer(data, params, layout)
  },
  # the only bin decision this package still makes: stat_bin's own fallback is
  # bins = 30, which renders integer data as slivers, so a caller who named no
  # grid at all -- no breaks, no binwidth, no bins -- gets a countable one instead.
  # Mirrors fix_bin_params()'s own condition for when it falls back to bins = 30,
  # so the two cannot drift on when a default applies.
  setup_params = function(self, data, params) {
    if (is.null(params$breaks %||% params$binwidth %||% params$bins)) {
      values <- data$x[!is.na(data$x)]
      params$binwidth <- if (length(values) == 0) 1 else squareplot_binwidth(values)
    }
    ggplot2::ggproto_parent(ggplot2::StatBin, self)$setup_params(data, params)
  }
)

#' Count a discrete x the way `geom_bar()` does
#'
#' A histogram's own answer for a discrete x is `stat_count`, not a binning
#' arithmetic of ours: `stat_bin` refuses a factor outright, and its refusal
#' names this pairing. The only reason this ggproto exists at all is to keep
#' the panel y-scale guard reachable at build time for the discrete path too --
#' every other member, including `compute_group` and `parameters()`, is
#' [ggplot2::StatCount] untouched.
#'
#' @format A [ggplot2::Stat] object.
#'
#' @noRd
StatSquareplotCount <- ggplot2::ggproto(
  "StatSquareplotCount", ggplot2::StatCount,
  compute_layer = function(self, data, params, layout) {
    squareplot_check_panels(data, layout)
    ggplot2::ggproto_parent(ggplot2::StatCount, self)$compute_layer(data, params, layout)
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
#' The geom takes either a bin's edges (`xmin`/`xmax`, from `ggplot2::stat_bin()`)
#' or a level's position plus a column width (`x`/`width`, from
#' `ggplot2::stat_count()`), deriving the edges it was not given. Paired with
#' `stat = "count"`, it draws one countable column per category the way
#' `geom_bar()` draws one bar.
#'
#' The `bars` parameter chooses what a bin is drawn as, on one grid and in one
#' layer: `"none"` draws the squares, `"outline"` frames them with the bar they
#' add up to, and `"solid"` draws that bar with the squares covered over. The
#' bar is derived from the squares rather than re-binned, so a bin holding no
#' observations has no bar, and a solid bar stacks a mapped `fill` in the same
#' spans its squares did. `bar_color` and `bar_linewidth` are the bar's own
#' color and width; `color` and `linewidth` are the separators between the
#' squares, and either may be mapped.
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
    # StatBin supplies xmin/xmax itself -- a bin's edges -- and those are honored
    # untouched. StatCount supplies neither, only a level's position and a column
    # width, so a counted x needs the same derivation stat_bin's own bin_out()
    # uses for its default edges: x is the center, the column is width wide.
    if (is.null(data$xmin) || is.null(data$xmax)) {
      half <- data$width / 2
      data$xmin <- data$x - half
      data$xmax <- data$x + half
    }
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
    params$bars <- params$bars %||% "none"
    if (params$bars %in% c("none", "outline", "solid") == FALSE) {
      abort(c(
        "`bars` must be one of \"none\", \"outline\" or \"solid\"",
        glue("found: {deparse(params$bars)}")
      ))
    }
    ggplot2::ggproto_parent(ggplot2::GeomRect, self)$setup_params(data, params)
  },
  draw_panel = function(self, data, panel_params, coord, bars = "none",
                        # ggplot2 rewrites "color" to "colour" anywhere in a parameter
                        # name before a geom sees it, so the documented `bar_color`
                        # arrives spelled this way and this formal cannot be renamed
                        bar_colour = NULL, bar_linewidth = 0.5,
                        lineend = "butt", linejoin = "mitre") {
    if (nrow(data) == 0) {
      return(ggplot2::zeroGrob())
    }

    bar_grob <- NULL
    if (!identical(bars, "none")) {
      # the solid bar replaces the squares, so it has to show what they showed:
      # one span per group in the bin, stacked, rather than one rectangle per bin
      rows <- squareplot_bar_rows(data, by_group = identical(bars, "solid"))
      rows$colour <- bar_colour %||% "grey20"
      rows$linewidth <- bar_linewidth
      rows$linetype <- 1
      if (identical(bars, "outline")) {
        rows$fill <- NA
        rows$alpha <- NA
      }
      bar_grob <- ggplot2::GeomRect$draw_panel(
        rows, panel_params, coord, lineend = lineend, linejoin = linejoin
      )
    }
    if (identical(bars, "solid")) {
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
      # one height per square, not one for the layer. A non-linear coord draws
      # the same one-count square shorter the higher it sits, and it does that
      # after this hook runs -- `data` here is still in count space -- so ask
      # the coord where these corners land before measuring them. A border
      # fitted to the bottom square would swallow every square above it.
      height_frac = squareplot_drawn_heights(data, panel_params, coord),
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
  # pmin, not min: `height_pt` carries one height per square once a non-linear
  # coord has had its say, and a border is fitted to the square it borders
  pmin(requested, pmin(width_pt, height_pt) / 4)
}

#' How tall each square is once the coord has had its say
#'
#' `draw_panel()` runs before the coord distorts anything, so a square's corners
#' arrive in count space and every square looks one count tall. Under a linear
#' coord that is also what gets drawn. Under `coord_transform()` it is not: the
#' same one count is drawn shorter the higher up the stack it sits, which is the
#' picture a transformed count axis exists to show. Asking the coord to place
#' the corners is the only way to measure the squares that will actually appear.
#'
#' @param data The layer's data, in count space.
#' @param panel_params The panel's parameters.
#' @param coord The plot's coord.
#'
#' @return One height per square, as a fraction of the panel.
#'
#' @noRd
squareplot_drawn_heights <- function(data, panel_params, coord) {
  placed <- tryCatch(
    coord$transform(data[c("x", "y", "ymin", "ymax")], panel_params),
    error = function(cnd) NULL
  )
  # a coord that will not place bare corners (or has no y to place) leaves the
  # count-space height, which is what a linear coord would have answered anyway
  if (is.null(placed) || is.null(placed$ymin) || is.null(placed$ymax)) {
    return((data$ymax - data$ymin) / diff(panel_params$y.range))
  }
  # every coord places corners in npc, so the heights come back as panel
  # fractions already -- which is exactly what the border fit wants
  placed$ymax - placed$ymin
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
  smaller <- pmin(width_pt, height_pt)

  # count the squares a fixed border really covers, rather than deciding from
  # the first one: under a coord transform they are not all the same height
  hidden <- sum(stroke >= smaller)
  if (!x$fit && hidden > 0) {
    warn(c(
      glue(
        "{hidden} observation{if (hidden == 1) '' else 's'} hidden behind ",
        "{if (hidden == 1) 'its' else 'their'} own border: ",
        "the smallest square is {signif(min(smaller), 2)} pt across ",
        "but its border is {signif(max(stroke), 2)} pt wide"
      ),
      i = "leave `linewidth` unset to fit the border to the square"
    ), class = "coursekata_squares_hidden")
  }

  rects <- x$children[[1]]
  rects$gp$lwd <- stroke
  grid::setChildren(x, grid::gList(rects))
}
