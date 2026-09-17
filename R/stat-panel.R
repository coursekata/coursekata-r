#' Refuse aesthetics that a panel-wide stat cannot honor
#'
#' @param data The mapped data passed to a stat.
#' @param constructor The public constructor's name.
#' @param positions Position aesthetics the stat consumes.
#'
#' @return `NULL`, invisibly.
#' @noRd
check_panel_stat_aesthetics <- function(data, constructor, positions) {
  mapped <- setdiff(names(data), c(positions, "PANEL", "group"))
  if (length(mapped) > 0) {
    mapped <- paste0("`", mapped, "`")
    abort(c(
      glue("`{constructor}()` computes once per panel, so {collapse(mapped)} can't be mapped"),
      i = "Set the aesthetic to one value, or facet the plot to compute one result per group."
    ))
  }
  invisible(NULL)
}
