#' Compute a distribution's mean, per panel
#'
#' `StatDistMean` receives the rows that ggplot2 has assigned to each panel and
#' emits their mean as an `xintercept`. Because ggplot2 transforms position
#' scales and applies hard-limit out-of-bounds handling before a stat runs, the
#' result follows that lifecycle: a transformed scale changes the values being
#' averaged, hard scale limits can remove values, and coordinate zoom does not
#' change the result.
#'
#' `stat_dist_mean()` is the conventional layer constructor. It computes from
#' `x` and defaults to a vertical line; [show_mean()] is the teaching-oriented
#' helper that resolves a distribution plot's mapping and styles this layer.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object used to display the data. Defaults to
#'   `"vline"`.
#' @param position A position adjustment. Defaults to `"identity"`.
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE`, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If `FALSE`, override the default aesthetics rather than
#'   combining with them.
#'
#' @return `stat_dist_mean()` returns a ggplot2 layer.
#'
#' @format `StatDistMean` is a [ggplot2::Stat] object.
#'
#' @seealso [show_mean()] and [gf_model()].
#' @export
StatDistMean <- ggplot2::ggproto(
  "StatDistMean", ggplot2::Stat,
  required_aes = "x",
  compute_panel = function(self, data, scales) {
    axis <- self$required_aes
    result <- data.frame(mean(data[[axis]], na.rm = TRUE))
    names(result) <- paste0(axis, "intercept")
    result
  }
)

#' @rdname StatDistMean
#' @export
stat_dist_mean <- function(mapping = NULL, data = NULL, geom = "vline",
                           position = "identity", ..., na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatDistMean, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Make a distribution-mean stat for an inferred model axis
#'
#' `required_aes` is ggproto metadata, so model inference needs a distinct
#' subclass when the outcome is on y. The public `stat_dist_mean()` constructor
#' remains fixed to x and returns a layer.
#'
#' @param axis `"x"` or `"y"`.
#'
#' @return A `StatDistMean` ggproto instance whose required aesthetic is
#'   `axis`.
#' @noRd
new_stat_dist_mean <- function(axis = "x") {
  ggplot2::ggproto(NULL, StatDistMean, required_aes = axis)
}
