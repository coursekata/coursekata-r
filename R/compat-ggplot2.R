# Everything this package does differently because of the ggplot2 version lives
# here, and nowhere else. DESCRIPTION's floor is not a formality: the browser
# environment students actually use (jupyterlite/app/pixi.lock) installs
# ggplot2 3.5.2 with ggformula 0.12.2, because ggplot2 4.x depends on S7 and S7
# has no WebAssembly build. Until that changes, 3.5.2 is a target we ship to.
#
# WHEN THE FLOOR MOVES TO 4.0: delete this file, delete its test file, and
# inline each helper's 4.0 branch at the call sites listed in its comment. That
# is the whole removal -- if a version check appears anywhere else, it is in the
# wrong place and belongs here instead.

#' Is the loaded ggplot2 at least this version?
#'
#' @param version A version string, e.g. `"4.0.0"`.
#'
#' @return `TRUE` when the installed ggplot2 is at or above `version`.
#'
#' @noRd
ggplot2_at_least <- function(version) {
  utils::packageVersion("ggplot2") >= package_version(version)
}

#' The name a coord transformation goes by in the loaded ggplot2
#'
#' ggplot2 4.0 renamed `coord_trans()` to `coord_transform()`. It is the same
#' coord: a squareplot drawn under either one distorts its squares identically,
#' so this is a spelling difference and not a capability difference.
#'
#' Naming it in a refusal matters because the message tells a reader what to
#' write next, and advice that names a function their ggplot2 does not export
#' sends them somewhere they cannot go.
#'
#' Called by: `squareplot_check_y_scale()` in R/gf_squareplot.R, and the tests
#' that assert what that refusal says.
#'
#' @return `"coord_transform"` or `"coord_trans"`.
#'
#' @noRd
coord_transform_name <- function() {
  if (ggplot2_at_least("4.0.0")) "coord_transform" else "coord_trans"
}

#' Build a coord transformation without caring what it is called
#'
#' The counterpart to `coord_transform_name()` for code that has to construct
#' one rather than name one -- used by the tests that check a square survives
#' a distorted axis.
#'
#' @param ... Passed to `coord_transform()` or `coord_trans()`.
#'
#' @return A ggproto Coord.
#'
#' @noRd
coord_transform_compat <- function(...) {
  getExportedValue("ggplot2", coord_transform_name())(...)
}

#' Does ggplot2's own jitter restart its draws at every panel?
#'
#' [ggplot2::PositionJitter] answers this differently in the two releases this
#' package runs on, and a residual has to answer it the same way its points
#' layer does or the two land on different offsets. In 3.5.2 the position
#' implements `compute_layer` and jitters the whole layer in one sequence; in
#' 4.0 it implements `compute_panel`, so ggplot2's own parent splits the layer
#' and re-seeds inside each panel. Neither is more correct -- what matters is
#' that both layers of a plot agree, and the points layer is not ours to
#' choose.
#'
#' Asked of the object rather than of a version number, because the question
#' is exactly "which hook does upstream implement" and the object can answer
#' it. `names()` on a ggproto lists what it defines itself, not what it
#' inherits, so a release that moves the behavior again is followed without an
#' edit here.
#'
#' Called by: `PositionResidJitter`'s `compute_layer` in R/geom-resid.R.
#'
#' @return `TRUE` when upstream jitters per panel.
#'
#' @noRd
jitter_is_per_panel <- function() {
  "compute_panel" %in% names(ggplot2::PositionJitter)
}
