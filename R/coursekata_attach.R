#' Attach the CourseKata course packages
#'
#' @param startup Is this being run at start-up?
#'
#' @return A coursekata attachments object with info about which course packages are installed and
#'   attached.
#' @export
#'
#' @examples
#' coursekata_attach()
coursekata_attach <- function(startup = FALSE) {
  to_attach <- coursekata_detached()
  if (length(to_attach)) {
    suppressPackageStartupMessages(purrr::walk(to_attach, pkg_require))
  }

  coursekata_attachments(startup)
}


#' Information about CourseKata packages.
#'
#' @param startup Is this being run at start-up?
#'
#' @return A coursekata_attachments object, also of class data.frame with a row for each course
#'   package and a column for each of the `package` name, `version`, and whether it is currently
#'   `attached`.
#' @keywords internal
coursekata_attachments <- function(startup = FALSE) {
  is_dark_theme <- rstudioapi::isAvailable() &&
    rstudioapi::hasFun("getThemeInfo") &&
    rstudioapi::getThemeInfo()$dark
  theme <- themes[[if (!is_dark_theme) "dark" else "light"]]

  info <- coursekata_packages()
  version <- ifelse(is.na(info$version), "", info$version)
  pkgs <- theme$pkg(paste(
    ifelse(info$attached, theme$good(cli::symbol$tick), theme$bad('x')),
    theme$text(format(info$package)),
    cli::ansi_align(version, max(cli::ansi_nchar(version)))
  ))

  msg <- paste(
    cli::rule(
      left = cli::style_bold("CourseKata packages"),
      right = cli::style_bold("coursekata ", utils::packageVersion("coursekata"))
    ),
    to_cols(pkgs, 2),
    sep = "\n"
  )

  if (startup) packageStartupMessage(msg) else message(msg)

  invisible(info)
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
