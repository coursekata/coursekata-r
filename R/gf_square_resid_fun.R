#' Add Squared Residual Visualization from a Function to a Plot
#'
#' `r lifecycle::badge("experimental")`
#'
#' Draws squared residual polygons between observed points and the values
#' predicted by a user-supplied function of x. Where [gf_square_resid()]
#' measures a fitted model, this measures a function you wrote: it is called on
#' the x values the plot draws, and each square is built on the residual from an
#' observation to what the function predicts for it.
#'
#' @param object A ggformula plot object, typically created with `gf_point()`.
#' @param fun A function of one argument. It is called on the x values the plot
#'   draws and must return one predicted y for each of them. May be given
#'   positionally or as `fun =`.
#' @param aspect The square's aspect ratio. Default is `4/6`. Must be named.
#' @param alpha The transparency of the square's fill. Default is `0.1`. Must be named.
#' @param gformula Not used. `gf_square_resid_fun()` measures a function, not an
#'   aesthetic formula; a function given positionally lands here and is moved to
#'   `fun`.
#' @param data Not used. The residuals are measured over the data the plot was
#'   built from. Anything supplied here is left for ggformula and ggplot2 to
#'   answer, exactly as it is for any other `gf_` layer.
#' @param ... Additional arguments. Typically these are (a) ggplot2 aesthetics to be set with
#'   `attribute = value`, such as `color` or `fill`, (b) ggplot2 aesthetics to be mapped with
#'   `attribute = ~ expression`, or (c) attributes of the layer as a whole.
#' @param xlab,ylab,title,subtitle,caption Labels for the plot.
#' @param geom,stat,position Not set by the caller. A squared residual is drawn
#'   by its own geom and stat, and moved by the position the observations are
#'   already drawn with, so that a square stays on the point it belongs to.
#' @param show.legend Whether this layer contributes to the legend.
#' @param show.help Print the layer's own help instead of drawing.
#' @param inherit Whether the layer inherits the plot's aesthetics. `FALSE`,
#'   where [gf_resid_fun()] is `TRUE`: a square is a filled region drawn in the
#'   geom's own colors, so inheriting a plot's mapped `color` outlines every
#'   square in the color of the group it measures instead of leaving one neutral
#'   area per observation. The axes and the prediction are stated outright, so
#'   nothing the square needs is lost by not inheriting. Set it to `TRUE` to take
#'   the outline anyway.
#' @param environment The environment mappings are resolved in.
#'
#' @return A ggplot object with squared residual polygons added.
#'
#' @export
#' @importFrom ggformula layer_factory
#' @examples
#' set.seed(1)
#' df <- data.frame(X = 1:10, Y = 2 + 3 * (1:10) + rnorm(10))
#' my_fun <- function(x) 2 + 3 * x
#'
#' gf_point(Y ~ X, data = df) %>%
#'   gf_function(my_fun) %>%
#'   gf_square_resid_fun(my_fun, color = "red", alpha = 0.3)
gf_square_resid_fun <- ggformula::layer_factory(
  # package-qualified so `::` resolves it whether or not coursekata is attached;
  # see the note above `gf_squareplot()`'s `layer_factory()` call
  geom = coursekata::GeomSquareResid,
  stat = coursekata::StatResid,
  # a placeholder; `pre` swaps in the position the observations are already drawn with
  position = "identity",
  # `gf_square_resid_fun()` measures a function, not an aesthetic formula. See
  # `gf_resid_fun()`; NULL is what makes a bare call print
  # "gf_square_resid_fun() does not require a formula."
  aes_form = NULL,
  # FALSE where the segment variants are TRUE, and measured rather than copied:
  # inheriting a mapped color outlines every square in its group's color instead
  # of leaving one neutral area. See `gf_square_resid()` for the full measurement
  inherit.aes = FALSE,
  # no default on the model/function so `missing()` can tell "not supplied" from
  # "supplied as NULL"; the rest must be extras to survive the factory -- see
  # `gf_resid()` for why `...` would drop them
  extras = alist(fun = , aspect = 4 / 6, alpha = 0.1),
  note = "the function to measure: a function of x returning predicted y",
  # `pre` is evaluated in the ggformula namespace, so a coursekata helper needs :::
  pre = {
    # the second positional argument binds to `gformula`, but this function takes
    # a function there; take it back before anything reads it. The move is
    # unconditional -- see `gf_resid_fun()`'s own note on why there is no is.function()
    if (!missing(gformula) && missing(fun)) {
      fun <- gformula
      gformula <- NULL
    }

    # a bare call has to reach the help gate untouched, and `show.help` is NULL
    # until ggformula decides -- see `gf_resid()` for the full reasoning
    if ((!missing(object) || !missing(fun)) && !isTRUE(show.help)) {
      # behind the help gate (asking what a function takes is not using it) and
      # first inside it, which is where the bespoke function fired it
      lifecycle::signal_stage("experimental", "gf_square_resid_fun()")

      # The residual has to start where its point is drawn, and the point may be
      # jittered. Read that jitter and declare the same one on this layer rather
      # than replaying the plot's: two layers sharing a seed land identically.
      # An unseeded jitter has no offsets to share, so it is pinned here -- on
      # the plot this returns, never on the one the caller still holds.
      jitter <- coursekata:::resid_jitter(if (missing(object)) NULL else object)
      object <- jitter$plot

      # One call, so the order the refusals fire in lives in one place: not a
      # plot, no function, no x/y on the plot, then the prediction. There is no
      # outcome guard and there must not be one -- a function of x predicts y, so
      # `resid_end()` never runs and the end aesthetic is "yend" outright.
      # `resid_fun_spec()` returns only the data and the mapping precisely so
      # this function states its own geom, inherit and tag below.
      resid <- coursekata:::resid_fun_spec(
        object, if (missing(fun)) NULL else fun, "gf_square_resid_fun"
      )
      # fill it only when the caller left it alone: overwriting swallows the
      # refusal ggformula already makes for a stray positional argument
      if (missing(data)) data <- resid$data
      aesthetics <- resid$aesthetics

      # a residual is these three or it is a different picture, so state them
      # rather than leave the caller a way to swap them (see `gf_resid()`)
      geom <- coursekata::GeomSquareResid
      stat <- coursekata::StatResid
      position <- jitter$position

      # here rather than at the factory: it needs this call's mapping, and a
      # factory-level `layer_fun` would tie this file's collation order to geom-resid.R's
      layer_fun <- coursekata:::resid_layer_fun("square_resid", resid$aesthetics)
    }
  }
)
