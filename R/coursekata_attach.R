#' Attach the CourseKata course packages
#'
#' @param do_not_ask Prevent asking the user to install missing packages (they are skipped).
#' @param quietly Whether to suppress messages.
#'
#' @return A named logical vector indicating which packages were attached.
#'
#' @export
#' @examples
#' coursekata_attach()
coursekata_attach <- function(do_not_ask = FALSE, quietly = FALSE) {
  !do_not_ask && pkg_check_installed(coursekata_pkgs)
  detached <- coursekata_detached()
  installed <- coursekata_pkgs[pkg_is_installed(coursekata_pkgs)]
  attached <- pkg_require(detached[detached %in% installed], quietly = quietly)

  result <- rep_named(detached, FALSE)
  result[names(attached)] <- TRUE
  invisible(result)
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
  if (length(pkgs) == 0) {
    return(NULL)
  }

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


#' List all currently NOT attached CourseKata course packages
#'
#' @return A character vector of the course packages that are not attached.
#'
#' @noRd
coursekata_detached <- function() {
  coursekata_pkgs[!pkg_is_attached(coursekata_pkgs)]
}
