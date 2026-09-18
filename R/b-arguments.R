#' Warn when the caller wrote something no mark `gf_b()` draws can honor
#'
#' The component geoms have distinct appearance controls and no legend.
#' Reject unused arguments at either public door before constructing marks.
#'
#' @param dots Named list, the caller's `...`, with the British spellings
#'   `colour`/`label_colour` already read out of it by the caller.
#' @param show_legend The caller's `show.legend`, `NA` when never set.
#' @param fn The name to warn in, `"gf_b"` or `"gf_coef"`.
#'
#' @return `NULL`, invisibly.
#'
#' @noRd
b_warn_unreachable <- function(dots, show_legend, fn) {
  unreachable <- names(dots)
  if (!isTRUE(is.na(show_legend))) {
    unreachable <- c(unreachable, "show.legend")
  }
  if (length(unreachable) == 0) {
    return(invisible(NULL))
  }
  warn(
    c(
      glue(
        "`{fn}()` places its own marks, so {collapse(paste0('`', unreachable, '`'))} ",
        "cannot reach them"
      ),
      "i" = paste(
        "set appearance with `color`, `label_color`, `label_size`,",
        "`arrow_linewidth`, `b0_linewidth`, `b0_size`, `b0_alpha`"
      )
    ),
    class = "coursekata_gf_b_unreachable"
  )
  invisible(NULL)
}
