#' Attach the CourseKata course packages
#' @return The packages that were attached.
#' @export
#' @examples
#' coursekata_attach()
coursekata_attach <- function() {
  to_attach <- coursekata_detached()
  suppressPackageStartupMessages(pkg_require(to_attach))
  invisible(to_attach)
}


#' Information about CourseKata packages.
#' @param pkgs A character vector of packages being loaded.
#' @return A coursekata_attachments object, also of class data.frame with a row for each course
#'   package and a column for each of the `package` name, `version`, and whether it is currently
#'   `attached`.
#' @keywords internal
coursekata_attach_message <- function(pkgs) {
  if (length(pkgs) == 0) return(NULL)

  is_dark_theme <- rstudioapi::isAvailable() &&
    rstudioapi::hasFun("getThemeInfo") &&
    rstudioapi::getThemeInfo()$dark
  theme <- themes[[if (is_dark_theme) "dark" else "light"]]

  info <- coursekata_packages()
  version <- ifelse(is.na(info$version), "", info$version)
  pkgs <- theme$pkg(paste(
    ifelse(info$attached, theme$good(cli::symbol$tick), theme$bad("x")),
    theme$text(format(info$package)),
    cli::ansi_align(version, max(cli::ansi_nchar(version)))
  ))

  paste(
    cli::rule(
      left = cli::style_bold("CourseKata packages"),
      right = cli::style_bold("coursekata ", utils::packageVersion("coursekata"))
    ),
    to_cols(pkgs, 2),
    sep = "\n"
  )
}


themes <- list(
  light = list(
    text = cli::col_black,
    pkg = cli::col_blue,
    good = cli::col_green,
    bad = cli::col_red
  ),
  dark = list(
    text = cli::col_white,
    pkg = cli::col_br_blue,
    good = cli::col_br_green,
    bad = cli::col_br_red
  )
)
