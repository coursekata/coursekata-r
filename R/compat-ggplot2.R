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
#' one rather than name one -- the reference examples and the tests that check a
#' square survives a distorted axis.
#'
#' @param ... Passed to `coord_transform()` or `coord_trans()`.
#'
#' @return A ggproto Coord.
#'
#' @noRd
coord_transform_compat <- function(...) {
  getExportedValue("ggplot2", coord_transform_name())(...)
}
