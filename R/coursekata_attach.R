#' Attach the CourseKata course packages
#'
#' @param quietly Whether to suppress messages.
#'
#' @return A named logical vector indicating which packages were attached.
#'
#' @export
#' @examples
#' coursekata_attach()
coursekata_attach <- function(quietly = FALSE) {
  to_attach <- coursekata_detached()
  invisible(pkg_require(to_attach, quietly = quietly))
}


#' Information about CourseKata packages.
#'
#' @param pkgs A character vector of packages being loaded.
#'
#' @return A coursekata_attachments object, also of class data.frame with a row for each course
#'   package and a column for each of the `package` name, `version`, and whether it is currently
#'   `attached`.
#'
#' @noRd
coursekata_attach_message <- function(pkgs) {
  if (length(pkgs) == 0) return(NULL)

  info <- coursekata_packages()
  version <- ifelse(is.na(info$version), "", info$version)
  pkgs <- paste(
    ifelse(info$attached, cli::col_green(cli::symbol$tick), cli::col_red("x")),
    cli::col_green(format(info$package)),
    cli::ansi_align(version, max(cli::ansi_nchar(version)))
  )

  paste(
    cli::rule(
      left = cli::style_bold("CourseKata packages"),
      right = cli::style_bold("coursekata ", utils::packageVersion("coursekata"))
    ),
    to_cols(pkgs, 2),
    sep = "\n"
  )
}
